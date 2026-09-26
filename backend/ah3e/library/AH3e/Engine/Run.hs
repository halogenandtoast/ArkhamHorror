module AH3e.Engine.Run (runMessage) where

import AH3e.Content
import AH3e.Engine.Behavior
import AH3e.Engine.Effect
import AH3e.Engine.Helpers
import AH3e.Engine.Hooks
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Engine.Setup (availableScenarios, setupScenario)
import AH3e.Engine.Test
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State
import Data.List (findIndex, nub)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

runMessage :: Message -> GameM ()
runMessage msg = case msg of
  -- Choose scenario (rule 101)
  ChooseScenario -> do
    expansions <- use #expansions
    host <- uses #players (fromJustNote "no players" . listToMaybe . map (.id))
    let options = availableScenarios expansions
    ask host "Choose a scenario"
      $ [Choice (ScenarioLabel sc.code) [SelectScenario sc.code] | sc <- options]
      <> [label "Random scenario" [RandomScenario] | length options > 1]
  RandomScenario -> do
    expansions <- use #expansions
    pickRandom (availableScenarios expansions) >>= traverse_ (push . SelectScenario . (.code))
  SelectScenario code -> do
    let sc = fromJustNote ("unknown scenario " <> show code) (scenarioDef code)
    #scenario ?= code
    logText ("Scenario: " <> sc.name)
    setupScenario sc
  -- Setup (rules 110-111)
  AskInvestigatorChoice -> do
    ps <- use #players
    let waiting = [p.id | p <- ps, isNothing p.investigator]
    if null waiting
      then do
        order <- playerOrder
        invs <- catMaybes <$> traverse investigatorOfPlayer order
        pushAll
          $ concat
            [ [ GainStartingPossessions iid ((fromJustNote "def" (investigatorDef iid)).starting)
              , PlaceAtStartingSpace iid
              ]
            | iid <- invs
            ]
          <> [FinalPreparations]
      else do
        available <- availableInvestigators
        for_ waiting \pid ->
          ask
            pid
            "Choose your investigator"
            [ Choice (InvestigatorLabel d.id) [SelectInvestigator pid d.id, AskInvestigatorChoice]
            | d <- available
            ]
  SelectInvestigator pid iid -> do
    #players %= map (\p -> if p.id == pid then p & #investigator ?~ iid else p)
    #investigators . at iid ?= newInvestigator iid pid
    #usedInvestigators %= (<> [iid])
    d <- getInvestigatorDef iid
    logText (d.name <> " joins the investigation")
  GainStartingPossessions iid possessions -> for_ (reverse possessions) \case
    StartingCard code -> push (GainNamedStarting iid code)
    StartingMoney n -> addMoney iid n
    StartingRemnants n -> addRemnants iid n
    StartingClues n -> addClues iid n
    StartingCondition name -> push (GainConditionMsg iid name)
    StartingChoice options -> do
      pile <- startingPool
      choices <- for options \o -> do
        cids <- fmap concat $ for [code | StartingCard code <- o] \code ->
          take 1 <$> filterM (fmap (== code) . cardCode) pile
        pure $ Choice (CardsLabel (possessionsText o) cids) [GainStartingPossessions iid o]
      chooseFor iid "Choose a starting possession" choices
  GainNamedStarting iid code -> do
    pile <- use (#decks . #starting)
    matches <- filterM (fmap (== code) . cardCode) pile
    case matches of
      (cid : _) -> push (GainAsset iid cid)
      -- a possession that is also a deck card is the investigator's own copy,
      -- so it is minted rather than taken, and the deck keeps its own
      [] | isJust (cardDef code) -> newCard code >>= push . GainAsset iid
      [] -> logText ("Starting card unavailable: " <> tshow code)
  PlaceAtStartingSpace iid -> do
    start <- (.startingSpace) <$> getScenarioDef
    investigatorL iid . #space ?= start
    investigatorL iid . #status .= Playing
  FinalPreparations -> do
    sc <- getScenarioDef
    pushAll
      $ [SpawnClue, SpawnClue, SpawnClue]
      <> [PlaceDoomInOrder SourceRules sc.startingDoom | not (null sc.startingDoom)]
      <> [SpreadDoom]
      <> map AddArchiveToCodex sc.codex
      <> [SetupEncounterDecks, BeginRound]
  SetupEncounterDecks -> setupScenarioDecks
  -- Round structure (rule 200)
  BeginRound -> do
    #round += 1
    #rumorIgnored .= []
    #activatedMonsters .= []
    #terrorEncountered .= []
    #investigators %= Map.map \i -> i {performed = [], usedAssets = []}
    r <- use #round
    logText ("Round " <> tshow r)
    push BeginActionPhase
  BeginActionPhase -> do
    enterPhase ActionPhase
    start <- (.startingSpace) <$> getScenarioDef
    joining <- uses #investigators (filter ((== Joining) . (.status)) . Map.elems)
    for_ joining \i -> do
      investigatorL i.id . #space ?= start
      investigatorL i.id . #status .= Playing
      void (noticedEngageOnEntry i.id start)
    #investigators %= Map.map \i -> i {active = True, actionsTaken = 0, bonusActions = 0, lockedAssets = []}
    push NextActionTurn
  NextActionTurn -> do
    candidates <- filter (.active) <$> playingInvestigators
    case candidates of
      [] -> push BeginMonsterPhase
      [i] -> push (StartActionTurn i.id)
      _ -> for_ candidates \i -> ask i.player "Take your turn" [Choice (InvestigatorLabel i.id) [StartActionTurn i.id]]
  StartActionTurn iid -> do
    #turn ?= iid
    investigatorL iid . #actionsTaken .= 0
    push (ActionTurn iid)
  ActionTurn iid -> do
    i <- getInvestigator iid
    if not (isPlaying i)
      then push (EndActionTurn iid)
      else do
        allowance <- actionAllowance iid
        let endTurn = Choice (DoneLabel "End turn") [EndActionTurn iid]
        if i.actionsTaken >= allowance
          then push (EndActionTurn iid)
          else
            if i.delayed
              then chooseFor iid "You are delayed" [label "Stand up (skips this action)" [StandUp iid], endTurn]
              else do
                actions <- legalActions iid
                -- a card or codex action is shown by its own name, not as "ComponentAction ..."
                components <- componentActionsFor iid
                let actionLabel = \case
                      ComponentAction ref n
                        | (name : _) <- [def.label | (r, k, def) <- components, r == ref, k == n] ->
                            TextLabel name
                      a -> ActionLabel a
                chooseFor
                  iid
                  actionPrompt
                  ([Choice (actionLabel a) [PerformAction iid a] | a <- actions] <> [endTurn])
  StandUp iid -> do
    investigatorL iid . #delayed .= False
    investigatorL iid . #actionsTaken += 1
    push (ActionTurn iid)
  PerformAction iid kind -> do
    legal <- legalActions iid
    unless (kind `elem` legal) $ error ("illegal action " <> show kind)
    investigatorL iid . #performed %= (<> [kind])
    investigatorL iid . #actionsTaken += 1
    performAction iid kind
  AfterAction iid kind -> do
    t <- use #turn
    ph <- use #phase
    pushAll
      $ [CheckReactions (AfterGatherResources iid) [] | kind == GatherResourcesAction]
      <> [CheckReactions (AfterResearchAction iid) [] | kind == ResearchAction]
      <> [ActionTurn iid | t == Just iid, ph == ActionPhase]
  EndActionTurn iid -> do
    investigatorL iid . #active .= False
    #turn .= Nothing
    push NextActionTurn
  -- Monster phase (rule 202)
  BeginMonsterPhase -> do
    enterPhase MonsterPhase
    #activatedMonsters .= []
    push MonsterActivationStep
  MonsterActivationStep -> do
    done <- use #activatedMonsters
    ready <- uses #monsters (filter (\m -> m.state == Ready && m.card `notElem` done) . Map.elems)
    case ready of
      [] -> do
        invs <- map (.id) <$> playingInvestigators
        push (MonsterAttackStep invs)
      _ ->
        chooseGroup
          "Choose the next monster to activate"
          [Choice (MonsterLabel m.card) [ActivateMonster m.card, MonsterActivationStep] | m <- ready]
  ActivateMonster mid -> do
    #activatedMonsters %= (<> [mid])
    ready <- isMonsterReady mid
    when ready do
      d <- monsterDef mid
      case d.activation of
        Hunter rule -> push (MonsterStep mid d.speed (TowardPrey rule))
        Patrol dest _ -> push (MonsterStep mid d.speed (TowardSpaces dest))
        Lurker eff -> do
          ctx <- monsterCtx mid
          push (ResolveEffect ctx eff)
        CustomActivation key -> case customActivation key of
          Just f -> f mid
          Nothing -> logText ("Missing monster activation: " <> key)
  MonsterStep mid remaining target -> do
    ready <- isMonsterReady mid
    m <- getMonster mid
    when (ready && remaining > 0) do
      targets <- case target of
        TowardSpaces rule -> ruleSpaces (Just mid) rule
        TowardPrey rule -> do
          prey <- ruleInvestigators rule
          noticed <- filterM (fmap not . monsterIgnores mid . (.id)) prey
          pure (mapMaybe (.space) noticed)
      closest <- closestTo m.space targets
      steps <- nub . concat <$> traverse (nextStepsToward m.space) closest
      chooseGroup
        "Choose where the monster moves"
        (spaceChoices steps \s -> [MoveMonsterTo mid s, MonsterStep mid (remaining - 1) target])
  MoveMonsterTo mid sid -> do
    monsterL mid . #space .= sid
    push (MonsterEngagesIn mid sid)
  MonsterEngagesIn mid sid -> do
    ready <- isMonsterReady mid
    when ready do
      here <- investigatorsAt sid
      present <- filterM (fmap not . monsterIgnores mid . (.id)) here
      prey <- activationPrey mid
      engageTargets mid present prey >>= \case
        Right is -> for_ is \i -> engage i.id mid
        Left pool ->
          chooseGroup
            "Choose the investigator the monster engages"
            [Choice (InvestigatorLabel i.id) [EngageMonster i.id mid] | i <- pool]
  MonsterAttackStep [] -> push MonsterReadyStep
  MonsterAttackStep (iid : rest) -> do
    ms <- map (.card) <$> engagedMonsters iid
    pushAll [MonstersAttack iid ms, MonsterAttackStep rest]
  MonstersAttack _ [] -> pure ()
  MonstersAttack iid [mid] -> push (MonsterAttacks mid iid)
  MonstersAttack iid ms ->
    chooseGroup
      "Choose the next monster to attack"
      [Choice (MonsterLabel m) [MonsterAttacks m iid, MonstersAttack iid (filter (/= m) ms)] | m <- ms]
  MonsterAttacks mid iid -> do
    playing <- investigatorIsPlaying iid
    engaged <- uses #monsters (maybe False (isEngagedWith iid) . Map.lookup mid)
    when (playing && engaged) do
      d <- monsterDef mid
      push (SufferHarm iid (SourceMonster mid) NormalHarm d.damage d.horror)
  MonsterReadyStep -> do
    ms <- uses #monsters (filter ((== Exhausted) . (.state)) . Map.elems)
    pushAll ([ReadyMonster m.card | m <- ms] <> [BeginEncounterPhase])
  ReadyMonster mid -> do
    m <- getMonster mid
    setMonsterState mid Ready
    push (MonsterEngagesIn mid m.space)
  ExhaustMonster mid -> do
    d <- monsterDef mid
    m <- getMonster mid
    let cannot = any (`elem` d.keywords) [Massive, Relentless] || (Shrouded `elem` d.keywords && m.state == Ready)
    unless cannot $ setMonsterState mid Exhausted
  EngageMonster iid mid -> engage iid mid
  DisengageMonster iid mid -> do
    m <- getMonster mid
    case m.state of
      Engaged is -> setMonsterState mid (if length is > 1 then Engaged (filter (/= iid) is) else Ready)
      _ -> pure ()
  CheckEngagement mid -> do
    m <- getMonster mid
    push (MonsterEngagesIn mid m.space)
  -- Encounter phase (rule 203)
  BeginEncounterPhase -> do
    enterPhase EncounterPhase
    push NextEncounterTurn
  NextEncounterTurn -> do
    candidates <- filter (not . (.active)) <$> playingInvestigators
    case candidates of
      [] -> push BeginMythosPhase
      [i] -> push (StartEncounterTurn i.id)
      _ -> for_ candidates \i ->
        ask i.player "Resolve your encounter" [Choice (InvestigatorLabel i.id) [StartEncounterTurn i.id]]
  StartEncounterTurn iid -> do
    #turn ?= iid
    restricted <- isRestrictedByEngagement iid
    if restricted
      then push (EndEncounterTurn iid)
      else do
        mnid <- investigatorNeighborhood iid
        encountered <- use #terrorEncountered
        terror <- case mnid of
          Just nid -> not . null . (.attachedTerror) <$> getNeighborhood nid
          Nothing -> pure False
        if terror && iid `notElem` encountered
          then pushAll [ResolveTerrorEncounter iid, EncounterOptions iid]
          else push (EncounterOptions iid)
  EncounterOptions iid -> do
    sid <- fromJustNote "no space" <$> investigatorSpace iid
    s <- getSpace sid
    special <- codexSpaceEncounter sid
    mnid <- investigatorNeighborhood iid
    anomaly <- case mnid of
      Just nid -> (.anomaly) <$> getNeighborhood nid
      Nothing -> pure False
    let deck = case s.kind of
          LocationSpace -> NeighborhoodDeck (fromJustNote "neighborhood" s.neighborhood)
          StreetSpace _ -> StreetDeck
          TravelRouteSpace _ -> TravelRouteDeck
          ThresholdSpace _ -> ThresholdDeck
          MysterySpace -> MysteryDeck sid
          SpecialSpace -> AnomalyDeck
    case (s.kind, special) of
      (SpecialSpace, Just eff) -> pushAll [ResolveEffect (EffectCtx iid SourceScenario Nothing) eff, EndEncounterTurn iid]
      (SpecialSpace, Nothing) -> push (EndEncounterTurn iid)
      _ -> pushAll [ResolveEncounterFrom iid (if anomaly then AnomalyDeck else deck), EndEncounterTurn iid]
  ResolveTerrorEncounter iid -> do
    #terrorEncountered %= (<> [iid])
    nid <- fromJustNote "neighborhood" <$> investigatorNeighborhood iid
    push (ResolveEncounterFrom iid (TerrorDeck nid))
  ResolveEncounterFrom iid deck -> resolveEncounter iid deck
  -- the card stays in view until its reader is done with it
  AcknowledgeEncounter ->
    use #encounter >>= traverse_ \enc ->
      chooseFor enc.investigator "Encounter resolved" [Choice (DoneLabel "Continue") []]
  FinishEncounter -> do
    who <- uses #encounter (fmap (.investigator))
    finishEncounter
    for_ who \iid -> push (CheckReactions (AfterEncounter iid) [])
  EndEncounterTurn iid -> do
    investigatorL iid . #active .= True
    #turn .= Nothing
    push NextEncounterTurn
  -- Mythos phase (rule 204)
  BeginMythosPhase -> do
    enterPhase MythosPhase
    order <- playerOrder
    push (MythosTurn order)
  MythosTurn [] -> push EndRound
  MythosTurn (p : ps) -> pushAll [DrawMythosToken p, DrawMythosToken p, MythosTurn ps]
  DrawMythosToken pid -> do
    cup <- use #cup
    when (null cup) do
      drawn <- use #drawnTokens
      #cup .= drawn
      #drawnTokens .= []
    cup' <- use #cup
    mi <- if null cup' then pure Nothing else Just <$> randomR (0, length cup' - 1)
    for_ mi \idx -> do
      let tok = cup' !! idx
      #cup .= take idx cup' <> drop (idx + 1) cup'
      #drawnTokens %= (<> [tok])
      #activeToken ?= tok
      logText ("Mythos: " <> tshow tok)
      -- the token is read, then put away; its effect resolves without it on show
      pushAll [AcknowledgeMythosToken pid tok, ClearActiveToken, ResolveMythosToken pid tok]
  ResolveMythosToken pid tok -> case tok of
    SpreadDoomToken -> push SpreadDoom
    SpawnMonsterToken -> push (SpawnMonsterAt Nothing False)
    ReadHeadlineToken -> investigatorOfPlayer pid >>= traverse_ (push . DrawHeadline)
    SpawnClueToken -> push SpawnClue
    GateBurstToken -> push GateBurst
    ReckoningToken -> reckoningSources >>= push . ResolveReckonings
    BlankToken -> investigatorOfPlayer pid >>= traverse_ (\iid -> push (CheckReactions (DrewBlankToken iid) []))
    SpreadTerrorToken -> do
      board <- use #board
      nids <- nub . mapMaybe (`spaceNeighborhood` board) <$> unstableSpaces
      chooseGroup
        "Choose the neighborhood to spread terror in"
        [label (coerce n) [SpreadTerror n] | n <- nids]
  EndRound -> do
    ps <- use #players
    invs <- use #investigators
    let needsNew =
          [ p.id
          | p <- ps
          , maybe
              True
              (\iid -> maybe True (\i -> i.status `elem` [Defeated, Devoured, Retired]) (Map.lookup iid invs))
              p.investigator
          ]
    pushAll (map ReplaceInvestigator needsNew <> [BeginRound])
  ReplaceInvestigator pid -> do
    available <- availableInvestigators
    if null available
      then push (LoseTheGame "No investigators remain")
      else
        ask
          pid
          "Choose a new investigator"
          [ Choice
              (InvestigatorLabel d.id)
              [SelectInvestigator pid d.id, GainStartingPossessions d.id d.starting]
          | d <- available
          ]
  -- Movement (rules 454, 455)
  MoveStep ms -> moveStep ms
  MoveInvestigator ms sid -> do
    from <- fromJustNote "space" <$> investigatorSpace ms.investigator
    board <- use #board
    investigatorL ms.investigator . #space ?= sid
    moveEngagedWatchers ms.investigator sid
    engaged <- enterWith ms sid
    unless engaged $ case borderHazard from sid board of
      Just hz | canContinue ms -> hazardPrompt ms hz
      _ -> push (MoveStep ms)
  UseTravelRoute ms sid -> do
    addMoney ms.investigator (-1)
    investigatorL ms.investigator . #space ?= sid
    moveEngagedWatchers ms.investigator sid
    engaged <- enterWith ms sid
    unless engaged $ push (MoveStep ms)
  MoveDirectly iid sid -> do
    allowed <- reachable [sid]
    unless (null allowed) do
      investigatorL iid . #space ?= sid
      moveEngagedWatchers iid sid
      void (noticedEngageOnEntry iid sid)
  EnterSpace iid sid -> void (noticedEngageOnEntry iid sid)
  -- Harm (rules 416, 442)
  SufferHarm iid src kind dmg hor -> do
    playing <- investigatorIsPlaying iid
    when (playing && (dmg > 0 || hor > 0))
      $ push (PreventDamage (HarmPlan iid src kind dmg hor Nothing Nothing) [])
  -- Prevention comes before the damage is assigned, and the card doing it may
  -- belong to anyone, so each holder is asked in turn. A prevention casts a spell
  -- of its own, which may ask questions or even defeat its caster, so it reports
  -- what it prevented through damagePrevented and this step runs again behind it.
  PreventDamage plan0 declined -> do
    prevented <- use #damagePrevented
    #damagePrevented .= 0
    let plan = plan0 & #damage .~ max 0 (plan0.damage - prevented)
    when (prevented > 0) $ logText ("Prevented " <> tshow prevented <> " damage")
    offers <- damagePreventionsFor plan
    case [(owner, r) | (owner, r) <- offers, r.key `notElem` declined] of
      [] -> push (HarmDamageStage plan)
      ((owner, r) : _) -> do
        let name = maybe "an investigator" (.name) (investigatorDef plan.investigator)
        chooseFor owner ("Prevent damage to " <> name <> "?")
          $ [ Choice (DoneLabel "Skip") [PreventDamage plan (r.key : declined)]
            , Choice (TextLabel r.label) (r.messages <> [PreventDamage plan (r.key : declined)])
            ]
  -- one asset at most per type: pick it (or nobody), then how much it takes
  -- 483.7-483.9: suffer the spell's horror first, less any remnants spent; a
  -- defeat on the way stops the cast before its test
  CastSpell iid cid next -> do
    i <- getInvestigator iid
    name <- (.name) <$> getCardDef cid
    cost <- maybe 0 (.spellHorror) <$> assetDef cid
    logText ("Casting " <> name)
    let most = min cost i.remnants
        blood = (investigatorBehavior iid).castWithDamage
        payWith k asDamage =
          let rest = cost - k
              spend = ["spend " <> tshow k <> " remnant" <> (if k == 1 then "" else "s") | k > 0]
              suffer = ["suffer " <> tshow rest <> (if asDamage then " damage" else " horror") | rest > 0]
           in Choice
                (TextLabel (capitalize (T.intercalate ", " (spend <> suffer))))
                [PayCastCost iid cid k asDamage next]
        capitalize t = T.toUpper (T.take 1 t) <> T.drop 1 t
        options = [payWith k asDamage | k <- [0 .. most], asDamage <- False : [True | blood, k < cost]]
    case options of
      [only] -> pushAll only.messages
      _ -> chooseFor iid ("Casting " <> name <> " costs " <> tshow cost <> " horror") options
  PayCastCost iid cid spent asDamage next -> do
    cost <- maybe 0 (.spellHorror) <$> assetDef cid
    addRemnants iid (negate spent)
    let rest = cost - spent
        bonus = (investigatorBehavior iid).paidCastLoreBonus
        paid = spent > 0 || (asDamage && rest > 0)
        boost = \case
          BeginTest ts | ts.casting == Just cid, ts.skill == Lore -> BeginTest ts {bonusDice = ts.bonusDice + bonus}
          m -> m
    when (paid && bonus > 0) $ logText ("+" <> tshow bonus <> " lore while casting")
    pushAll
      [ if asDamage
          then SufferHarm iid (SourceCard cid) NormalHarm rest 0
          else SufferHarm iid (SourceCard cid) NormalHarm 0 rest
      , ResumeCast iid cid (if paid then map boost next else next)
      ]
  ResumeCast iid cid next -> do
    playing <- investigatorIsPlaying iid
    when playing $ pushAll (next <> [CheckReactions (AfterCastSpell iid cid) []])
  HarmDamageStage plan -> do
    opts <- assignableAssets plan.investigator plan.kind (.health) (.damage)
    if plan.damage == 0 || null opts
      then push (HarmHorrorStage plan)
      else
        chooseFor plan.investigator ("Assign " <> tshow plan.damage <> " damage")
          $ label "Suffer it yourself" [HarmHorrorStage plan]
          : [ Choice (CardLabel cid) [HarmChooseAmount DamageStat cid (min plan.damage room) plan]
            | (cid, room) <- opts
            ]
  HarmHorrorStage plan -> do
    opts <- assignableAssets plan.investigator plan.kind (.sanity) (.horror)
    if plan.horror == 0 || null opts
      then push (ResolveHarm plan)
      else
        chooseFor plan.investigator ("Assign " <> tshow plan.horror <> " horror")
          $ label "Suffer it yourself" [ResolveHarm plan]
          : [ Choice (CardLabel cid) [HarmChooseAmount HorrorStat cid (min plan.horror room) plan]
            | (cid, room) <- opts
            ]
  HarmChooseAmount stat cid most plan -> do
    let next k = case stat of
          DamageStat -> HarmHorrorStage plan {damageTo = Just (cid, k)}
          HorrorStat -> ResolveHarm plan {horrorTo = Just (cid, k)}
        what = case stat of DamageStat -> "damage"; HorrorStat -> "horror"
    name <- (.name) <$> getCardDef cid
    if most <= 1
      then push (next most)
      else
        chooseFor plan.investigator ("How much " <> what <> " does " <> name <> " take?")
          $ [Choice (AmountLabel k) [next k] | k <- [most, most - 1 .. 1]]
  -- the whole assignment lands at once, so one asset can soak both kinds
  ResolveHarm plan -> do
    let taken = maybe 0 snd
        onAssets =
          Map.toList
            $ Map.fromListWith
              (\(d1, h1) (d2, h2) -> (d1 + d2, h1 + h2))
              ( [(cid, (k, 0)) | Just (cid, k) <- [plan.damageTo]]
                  <> [(cid, (0, k)) | Just (cid, k) <- [plan.horrorTo]]
              )
    soaked <- for onAssets \(cid, dh) -> (cid,) <$> preventOwnHarm plan.investigator cid dh
    pushAll
      $ [HarmAsset cid d h | (cid, (d, h)) <- soaked]
      <> [ ApplyHarm
             plan.investigator
             plan.source
             (plan.damage - taken plan.damageTo)
             (plan.horror - taken plan.horrorTo)
         , HarmResolved plan
         ]
  HarmResolved plan -> do
    playing <- investigatorIsPlaying plan.investigator
    when playing $ afterHarmFor plan >>= pushAll
    for_ [mid | SourceMonster mid <- [plan.source]] (feed plan)
  AddTestSuccesses n -> do
    inTest <- uses #test isJust
    if inTest then #test . _Just . #addedSuccesses += n else #pendingSuccesses += n
  HarmAsset cid dmg hor -> do
    assetL cid . #damage += dmg
    assetL cid . #horror += hor
    a <- use (assetL cid)
    md <- assetDef cid
    -- a 0 health or sanity means the asset can't take that kind at all (408.7), not that it's full
    let full value taken = maybe False (\v -> v > 0 && taken >= v) value
        dead = maybe False (\d -> full d.health a.damage || full d.sanity a.horror) md
    when dead $ push (DiscardAsset cid)
  ApplyHarm iid _ dmg hor -> do
    investigatorL iid . #damage += dmg
    investigatorL iid . #horror += hor
    push (CheckDefeat iid)
  CheckDefeat iid -> do
    i <- getInvestigator iid
    h <- investigatorHealth iid
    s <- investigatorSanity iid
    when (isPlaying i && (i.damage >= h || i.horror >= s)) $ push (DefeatInvestigator iid)
  DefeatInvestigator iid -> removeInvestigator iid Defeated True
  DevourInvestigator iid -> removeInvestigator iid Devoured True
  RetireInvestigator iid -> removeInvestigator iid Retired False
  RecoverInvestigator iid hp sp -> do
    investigatorL iid . #damage %= max 0 . subtract hp
    investigatorL iid . #horror %= max 0 . subtract sp
  RecoverAsset cid hp sp -> do
    assetL cid . #damage %= max 0 . subtract hp
    assetL cid . #horror %= max 0 . subtract sp
  -- Monsters (rule 453)
  DealMonsterDamage mid src n -> do
    exists <- uses #monsters (Map.member mid)
    when (exists && n > 0) do
      d <- monsterDef mid
      m <- getMonster mid
      blocked <- case src of
        SourceInvestigator iid | Relentless `elem` d.keywords -> (/= Just m.space) <$> investigatorSpace iid
        _ -> pure False
      case src of
        SourceInvestigator iid -> #provoked %= Map.insertWith (<>) mid [iid]
        _ -> pure ()
      unless blocked do
        monsterL mid . #damage += n
        mh <- effectiveMonsterHealth mid
        m' <- getMonster mid
        -- A card that answers damage (Lita Chantler) strikes after the defeat
        -- check, so a monster this damage already killed is simply no longer there
        -- to strike -- DealMonsterDamage no-ops for a monster out of play.
        answers <- afterMonsterDamagedFor mid src
        pushAll
          $ case mh of
            Just h | m'.damage >= h -> [DefeatMonster mid src]
            _ -> []
          <> answers
  DefeatMonster mid _ -> do
    logText "Monster defeated"
    answers <- codexAboutMonster (.afterMonsterDefeated) mid
    pushAll (DiscardMonster mid : answers)
  DiscardMonster mid -> do
    d <- monsterDef mid
    gone <- (.removedWhenDefeated) <$> monsterBehavior mid
    #monsters . at mid .= Nothing
    #provoked . at mid .= Nothing
    if
      -- a card may take a monster out of the game entirely, as the worshipers of
      -- Umordhoth are taken once they are dealt with
      | gone -> #decks . #removed %= (mid :)
      | d.epic -> #decks . #archive %= (mid :)
      | Shrouded `elem` d.keywords -> do
          deck <- use (#decks . #monster)
          #decks . #monster <~ shuffle (mid : deck)
      | otherwise -> #decks . #monster %= (mid :)
  SpawnMonsterAt mspace exhausted -> do
    deck <- use (#decks . #monster)
    for_ (drawBottom deck) \(mid, rest) -> do
      #decks . #monster .= rest
      d <- monsterDef mid
      spaces <- maybe (ruleSpaces (Just mid) d.spawn) (pure . pure) mspace
      case spaces of
        [] -> #decks . #monster %= (mid :)
        _ ->
          chooseGroup "Choose where the monster spawns"
            $ spaceChoices spaces \s -> [PlaceMonster mid s (if exhausted then Exhausted else Ready)]
  PlaceMonster mid sid state -> do
    #monsters . at mid ?= Monster {card = mid, space = sid, state, damage = 0}
    answers <- codexAboutMonster (.afterMonsterSpawn) mid
    pushAll (MonsterEngagesIn mid sid : answers)
  AttackDamage iid mid n -> do
    exists <- uses #monsters (Map.member mid)
    when exists do
      before <- (.damage) <$> getMonster mid
      pushAll [DealMonsterDamage mid (SourceInvestigator iid) n, AttackResolved iid mid before]
  AttackMonster iid mid -> do
    m <- getMonster mid
    d <- monsterDef mid
    unless (m.state == Exhausted) $ engage iid mid
    -- attacking provokes it even if the attack cannot engage it
    #provoked %= Map.insertWith (<>) mid [iid]
    let attackTest skill = newTest iid skill d.attackModifier (ActionTest AttackAction (Just mid)) (AfterAttack iid mid)
        attackWith skill = BeginTest (attackTest skill)
    -- a card like Storm of Spirits offers another skill in place of strength; the
    -- monster's attack modifier applies either way
    i <- getInvestigator iid
    alternatives <-
      if d.attackSkill /= Strength
        then pure []
        else fmap catMaybes $ for [c | c <- i.assets, c `notElem` i.lockedAssets] \c ->
          fmap (c,) . (.attackSkillInstead) <$> assetBehavior c
    if null alternatives
      then push (attackWith d.attackSkill)
      else do
        names <- for alternatives \(c, skill) -> (c,skill,) . (.name) <$> getCardDef c
        chooseFor iid "Choose the skill to test"
          $ label ("Test " <> T.toLower (tshow d.attackSkill)) [attackWith d.attackSkill]
          : [ Choice
                (CardsLabel ("Test " <> T.toLower (tshow skill) <> " with " <> name) [c])
                [CastSpell iid c [BeginTest (attackTest skill) {casting = Just c}]]
            | (c, skill, name) <- names
            ]
  AttackResolved iid mid before -> do
    mm <- use (#monsters . at mid)
    dealt <- case mm of
      Nothing -> do
        d <- monsterDef mid
        when d.remnant $ addRemnants iid 1
        pure True
      Just m -> pure (m.damage > before)
    gone <- uses #monsters (not . Map.member mid)
    retaliators <- filterM (hasKeyword Retaliate . (.card)) =<< engagedMonsters iid
    pushAll
      $ [MonsterAttacks r.card iid | r <- retaliators, r.card /= mid || not dealt]
      <> [CheckReactions (AfterDefeatMonsterInAttack iid) [] | gone]
  ClearSpaceDoom sid -> spaceL sid . #doom .= 0
  WardRemove iid sid n -> do
    s <- getSpace sid
    let k = min n s.doom
    when (k >= 2) $ addRemnants iid 1
    push (RemoveDoom sid k)
  PayMoney iid n -> addMoney iid (negate n)
  BuyFromDisplayMore ctx mtrait half limit ifBought n -> buyPrompt ctx mtrait half limit ifBought n
  EvadeMonsters iid n -> do
    ms <- map (.card) <$> engagedMonsters iid
    if n >= length ms
      then do
        for_ ms \mid -> pushAll [DisengageMonster iid mid, ExhaustMonster mid]
        unless (null ms) $ investigatorL iid . #bonusActions += 1
      else
        when (n > 0)
          $ chooseFor
            iid
            "Choose a monster to evade"
            [ Choice (MonsterLabel m) [DisengageMonster iid m, ExhaustMonster m, EvadeMonsters iid (n - 1)]
            | m <- ms
            ]
  -- Doom and clues (rules 406, 412, 423, 461)
  PlaceDoom src sid -> do
    s <- getSpace sid
    if
      | s.kind == SpecialSpace -> push (PlaceDoomOnSheet 1)
      | isStreetLike s.kind -> push (PlaceDoomInStreet src sid)
      | otherwise -> do
          let nid = fromJustNote "neighborhood" s.neighborhood
          n <- getNeighborhood nid
          if n.anomaly
            then push (PlaceDoomOnSheet 1)
            else do
              spaceL sid . #doom += 1
              pushAll [CheckDoomThresholds sid, CheckStateTriggers]
  PlaceDoomInStreet src sid -> do
    board <- use #board
    let adj =
          [ a
          | a <- adjacentSpaces sid board
          , maybe False (isNeighborhoodSpace . (.kind)) (Map.lookup a board.spaces)
          ]
    chooseGroup "Choose the neighborhood space for the doom" (spaceChoices adj \s -> [PlaceDoom src s])
  PlaceDoomInOrder _ [] -> pure ()
  PlaceDoomInOrder src [s] -> push (PlaceDoom src s)
  PlaceDoomInOrder src ss -> do
    let distinct = nub ss
    if length distinct == 1
      then pushAll (map (PlaceDoom src) ss)
      else
        chooseGroup
          "Choose where to place the next doom"
          (spaceChoices distinct \s -> [PlaceDoom src s, PlaceDoomInOrder src (deleteOne s ss)])
  PlaceDoomOnSheet n -> do
    #sheetDoom += n
    push CheckStateTriggers
  RemoveDoom sid n -> do
    s <- getSpace sid
    let k = min n s.doom
    spaceL sid . #doom -= k
    for_ s.neighborhood \nid -> do
      total <- uses #board (neighborhoodDoom nid)
      when (total == 0) $ neighborhoodL nid . #anomaly .= False
    push CheckStateTriggers
  CheckDoomThresholds sid -> checkDoomThresholds sid
  SpreadDoom -> withEventDeck \deck -> case drawBottom deck of
    Nothing -> pure ()
    Just (cid, rest) -> do
      #decks . #event .= rest
      #decks . #eventDiscard %= (cid :)
      #revealedEvent ?= cid
      #activeCard ?= cid
      e <- eventDef cid
      pushAll [PlaceDoomInOrder SourceMythos e.doomSpaces, ClearActiveCard cid]
  SpawnClue -> withEventDeck \case
    [] -> pure ()
    (cid : rest) -> do
      #decks . #event .= rest
      #revealedEvent ?= cid
      #activeCard ?= cid
      e <- eventDef cid
      neighborhoodL e.neighborhood . #clues += 1
      nd <- use (#decks . #neighborhoods . at e.neighborhood . non [])
      nd' <- shuffleIntoTopTwo cid nd
      #decks . #neighborhoods . at e.neighborhood ?= nd'
      logText ("A clue spawns in " <> coerce e.neighborhood)
      push (ClearActiveCard cid)
  GateBurst -> withEventDeck \case
    [] -> pure ()
    (cid : rest) -> do
      #revealedEvent ?= cid
      #activeCard ?= cid
      e <- eventDef cid
      discard <- use (#decks . #eventDiscard)
      shuffled <- shuffle (cid : discard)
      #decks . #event .= rest <> shuffled
      #decks . #eventDiscard .= []
      board <- use #board
      let spaces = neighborhoodSpaces e.neighborhood board
          expanded = any (\s -> maybe False ((== MysterySpace) . (.kind)) (Map.lookup s board.spaces)) spaces
      logText ("Gate burst in " <> coerce e.neighborhood)
      pushAll
        [ if expanded then ChooseGateBurstSpaces [] spaces else PlaceDoomInOrder SourceMythos spaces
        , ClearActiveCard cid
        ]
  ChooseGateBurstSpaces chosen remaining
    | length chosen >= 3 || null remaining -> push (PlaceDoomInOrder SourceMythos chosen)
    | otherwise ->
        chooseGroup
          "Choose a space for gate burst doom"
          (spaceChoices remaining \s -> [ChooseGateBurstSpaces (chosen <> [s]) (filter (/= s) remaining)])
  SpreadTerror nid -> do
    deck <- use (#decks . #terror)
    case deck of
      [] -> push (PlaceDoomOnSheet 1)
      (cid : rest) -> do
        #decks . #terror .= rest
        neighborhoodL nid . #terror += 1
        neighborhoodL nid . #attachedTerror %= (<> [cid])
  CheckStateTriggers -> checkStateTriggers
  -- Assets (rules 405, 408, 415, 446, 482, 483, 485)
  -- a card that bans conditions discards the ones its new owner already holds
  GainAsset iid cid -> do
    bans <- (.bansConditions) <$> assetBehavior cid
    for_ bans \name -> conditionCard iid name >>= traverse_ (push . DiscardAsset)
    removeCardEverywhere cid
    #assets
      . at cid
      ?= Asset
        { card = cid
        , owner = iid
        , damage = 0
        , horror = 0
        , flipped = False
        , attachedTo = Nothing
        , tokens = mempty
        }
    investigatorL iid . #assets %= (<> [cid])
    push RefillDisplay
  DiscardAsset cid -> discardAsset cid
  GainNamedCard iid name -> do
    pile <- use (#decks . #special)
    matches <- filterM (cardMatches (NamedCard name)) pile
    case matches of
      (cid : _) -> pushAll [GainAsset iid cid, AfterGainedFromDeck iid cid]
      [] -> logText ("Special card unavailable: " <> name)
  GainConditionMsg iid name -> gainCondition iid name
  FocusSkill iid skill evenIfExceeds -> do
    i <- getInvestigator iid
    let already = Map.findWithDefault 0 skill i.focus > 0
    unless already do
      investigatorL iid . #focus . at skill ?= 1
      checkFocusLimit iid evenIfExceeds
  FocusSkillAgain iid skill -> do
    investigatorL iid . #focus . at skill %= Just . maybe 1 (+ 1)
  DiscardFocus iid skill ->
    investigatorL iid . #focus . at skill %= \case
      Just n | n > 1 -> Just (n - 1)
      _ -> Nothing
  -- a headline that asked nothing would otherwise flash past unread
  AcknowledgeHeadline iid asked -> do
    now <- use #questionsAsked
    when (now == asked) $ chooseFor iid "Headline read" [Choice (DoneLabel "Continue") []]
  ClearActiveCard cid -> #activeCard %= \cur -> if cur == Just cid then Nothing else cur
  -- the token is drawn face up and read before anything happens
  AcknowledgeMythosToken pid tok ->
    ask pid ("Mythos: " <> mythosTokenName tok) [Choice (DoneLabel "Continue") []]
  ClearActiveToken -> #activeToken .= Nothing
  SpendSheetClues n -> #sheetClues %= max 0 . subtract n
  MarkSheet n -> do
    #sheetMarkers += n
    push CheckStateTriggers
  DiscardClue Nothing -> #sheetClues %= max 0 . subtract 1
  DiscardClue (Just iid) -> addClues iid (-1)
  ResearchClues iid n -> do
    i <- getInvestigator iid
    let maxN = min n i.clues
    if maxN <= 0
      then pure ()
      else
        chooseFor
          iid
          "Research clues"
          [Choice (AmountLabel k) [ResearchCluesExact iid k] | k <- [0 .. maxN]]
  ResearchCluesExact iid k -> do
    addClues iid (negate k)
    #sheetClues += k
    push CheckStateTriggers
  TradeWith iid other -> tradePrompt iid other
  TradeTransfer giver receiver item -> do
    case item of
      TradeMoney n -> addMoney giver (negate n) >> addMoney receiver n
      TradeClues n -> addClues giver (negate n) >> addClues receiver n
      TradeRemnants n -> addRemnants giver (negate n) >> addRemnants receiver n
      TradeCard cid -> do
        used <- elem cid . (.usedAssets) <$> getInvestigator giver
        investigatorL giver . #assets %= filter (/= cid)
        investigatorL receiver . #assets %= (<> [cid])
        assetL cid . #owner .= receiver
        when used $ investigatorL receiver . #lockedAssets %= (<> [cid])
  BuyFromDisplayMsg ctx mtrait half limit ifBought -> do
    let iid = ctx.investigator
        buy = BuyFromDisplayChecked ctx mtrait half limit ifBought
    markup <- displayMarkup iid
    if markup == 0
      then push buy
      else
        chooseFor
          iid
          "Truckers' Strike raises display prices by $2"
          [ label
              "Test influence −1 to ignore it this round"
              [ BeginTest (newTest iid Influence (-1) OtherTest (AfterEffect ctx (Custom "ignore-rumor") NoEffect))
              , buy
              ]
          , label "Buy at the raised prices" [buy]
          ]
  BuyFromDisplayChecked ctx mtrait half limit ifBought -> buyPrompt ctx mtrait half limit ifBought 0
  BuyCard iid cid price -> do
    addMoney iid (negate price)
    push (GainAsset iid cid)
  CycleDisplay iid n -> when (n > 0) do
    display <- use (#decks . #display)
    chooseFor iid "You may discard a card from the display"
      $ Choice (DoneLabel "Done") []
      : [ Choice (CardLabel c) [DiscardFromDisplay c, RefillDisplay, CycleDisplay iid (n - 1)] | c <- display
        ]
  DiscardFromDisplay cid -> do
    #decks . #display %= filter (/= cid)
    #decks . #item %= (<> [cid])
  RefillDisplay -> refillDisplay
  GainItemFromDeck iid kind mtrait mbound -> gainFromDeck iid kind mtrait mbound
  BuyRevealed ctx kind revealed limit half bought -> buyRevealed ctx kind revealed limit half bought
  ReturnToBottom kind cards -> assetDeckLens kind %= (<> cards)
  GainFromDisplay iid cid -> push (GainAsset iid cid)
  -- Codex (rules 407, 413)
  AddArchiveToCodex n -> addToCodex n False
  AddArchiveToCodexFlipped n -> addToCodex n True
  ChooseInvestigatorsFor ctx n candidates eff
    | n <= 0 || null candidates -> pure ()
    | otherwise ->
        chooseFor ctx.investigator "Choose an investigator"
          $ Choice (DoneLabel "Done") []
          : [ Choice
                (InvestigatorLabel c)
                [ ResolveEffect (ctx & #investigator .~ c) eff
                , ChooseInvestigatorsFor ctx (n - 1) (filter (/= c) candidates) eff
                ]
            | c <- candidates
            ]
  CheckReactions trigger used -> do
    available <- filter ((`notElem` used) . (.key)) <$> reactionsFor trigger
    unless (null available)
      $ chooseFor (triggerInvestigator trigger) "Use an ability?"
      $ Choice (DoneLabel "Skip") []
      : [ Choice (TextLabel r.label) (r.messages <> [CheckReactions trigger (r.key : used)]) | r <- available
        ]
  ContinueTest -> testPrompt
  MarkAssetUsed iid cid -> investigatorL iid . #usedAssets %= (<> [cid])
  FlipCodexCard n -> do
    #codex %= map (\e -> if e.number == n then e {flipped = not e.flipped, fired = []} else e)
    push CheckStateTriggers
    codexEntry n >>= traverse_ \e -> do
      logText ("Card " <> tshow (coerce n :: Int) <> " flips")
      (codexBehavior n).onFlip e
  {- A card that sends itself to the archive usually does so on the side it has just
  been turned to, which nobody has read yet, so the table says when it may go. A
  card leaving unflipped has shown nothing new and goes at once. -}
  RemoveCodexCard n -> do
    e <- codexEntry n
    case e of
      Just entry
        | entry.flipped ->
            askLeader
              ("Card " <> tshow (coerce n :: Int) <> " read")
              [Choice (DoneLabel "Continue") [DiscardCodexCard n]]
      _ -> push (DiscardCodexCard n)
  DiscardCodexCard n -> do
    entries <- use #codex
    for_ [e | e <- entries, e.number == n] \e -> #decks . #archive %= (e.card :)
    #codex %= filter ((/= n) . (.number))
  -- Headlines (rule 440)
  DrawHeadline iid -> do
    deck <- use (#decks . #headline)
    case deck of
      [] -> push (PlaceDoomOnSheet 1)
      (cid : rest) -> do
        #decks . #headline .= rest
        #activeCard ?= cid
        d <- getCardDef cid
        case d.kind of
          HeadlineCard h -> do
            logText ("Headline: " <> d.name)
            asked <- use #questionsAsked
            let ctx = EffectCtx {investigator = iid, source = SourceHeadline cid, testResult = Nothing}
            pushAll
              [ ResolveEffect ctx h.effect
              , AcknowledgeHeadline iid asked
              , DiscardHeadline cid
              , ClearActiveCard cid
              ]
          _ -> error "not a headline"
  DiscardHeadline cid -> do
    d <- getCardDef cid
    case d.kind of
      HeadlineCard h | h.rumor -> do
        old <- use #rumor
        for_ old \r -> #decks . #headlineDiscard %= (r.card :)
        #rumor ?= Rumor {card = cid, doom = 0}
        logText (d.name <> " is added to the codex")
      _ -> #decks . #headlineDiscard %= (cid :)
  DiscardRumor -> do
    old <- use #rumor
    for_ old \r -> do
      #decks . #headlineDiscard %= (r.card :)
      d <- getCardDef r.card
      logText (d.name <> " is discarded")
    #rumor .= Nothing
  AddRumorDoom -> do
    #rumor . _Just . #doom += 1
    old <- use #rumor
    for_ old \r -> do
      d <- getCardDef r.card
      case d.kind of
        HeadlineCard h | r.doom >= 3, isJust h.reckoning -> pushAll [GateBurst, DiscardRumor]
        _ -> pure ()
  OfferRumorDiscard ctx keep -> do
    payers <- filter ((> 0) . (.clues)) <$> playingInvestigators
    chooseGroup "Spend a clue to discard the rumor?"
      $ [ Choice
            (InvestigatorLabel i.id)
            [ResolveEffect (ctx & #investigator .~ i.id) (Pay (SpendClues 1) NoEffect), DiscardRumor]
        | i <- payers
        ]
      <> [label "Keep the rumor" [ResolveEffect ctx keep]]
  IgnoreRumor iid -> #rumorIgnored %= (iid :)
  ResolveReckonings [] -> pure ()
  -- askLeader, not chooseGroup: the last reckoning is asked for too, since the
  -- prompt is what points it out on the table
  ResolveReckonings ss ->
    askLeader
      "Choose the next reckoning to resolve"
      [Choice (SourceLabel s) [ResolveReckoning s, ResolveReckonings (filter (/= s) ss)] | s <- ss]
  ResolveReckoning src -> resolveReckoning src
  -- Effects and tests
  ResolveEffect ctx eff -> resolveEffect ctx eff
  PayCost ctx cost -> payCost ctx cost
  BeginTest ts -> beginTest ts
  ToggleTestAsset cid -> toggleTestAsset cid
  RollDice -> rollTestDice
  SpendForReroll cost -> chooseRerollDie cost
  RerollDie cost idx -> rerollDie cost idx
  RerollUpTo src n -> rerollUpTo src n
  RerollOneOf src n idx -> rerollOneOf src n idx
  RerollAll src -> rerollAll src
  AddToDie src -> chooseDieToRaise src
  RaiseDie idx -> raiseDie idx
  MarkUsedInTest cid -> markUsedInTest cid
  FinishTest -> finishTest
  -- End of game
  WinTheGame -> do
    #status .= Won
    #queue .= []
    logText "The investigators win"
  LoseTheGame reason -> do
    #status .= Lost reason
    #queue .= []
    logText ("The investigators lose: " <> reason)
  Debug action -> do
    runDebug action
    refreshActionPrompt
  AfterGainedFromDeck iid cid -> do
    b <- assetBehavior cid
    for_ b.afterGainedFromDeck \eff -> push (ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) eff)
  AskAboutAsset iid cid prompt choices -> do
    still <- elem cid . (.assets) <$> getInvestigator iid
    when still $ chooseFor iid prompt choices
  RestoreQuestions qs -> #questions %= (<> qs)
  LogText t -> logText t
 where
  deleteOne x = \case
    [] -> []
    (y : ys) -> if x == y then ys else y : deleteOne x ys
  canContinue ms = ms.remaining > 0 || ms.paidSteps < ms.maxPaidSteps

availableInvestigators :: GameM [InvestigatorDef]
availableInvestigators = do
  used <- use #usedInvestigators
  expansions <- use #expansions
  let usedNames = [d.name | iid <- used, Just d <- [investigatorDef iid]]
  pure
    [ d
    | d <- Map.elems investigatorDefs
    , d.expansion `elem` expansions
    , d.id `notElem` used
    , d.name `notElem` usedNames
    ]

monsterCtx :: CardId -> GameM EffectCtx
monsterCtx mid = do
  l <- leaderPlayer
  iid <- fromJustNote "leader investigator" <$> investigatorOfPlayer l
  pure EffectCtx {investigator = iid, source = SourceMonster mid, testResult = Nothing}

performAction :: InvestigatorId -> ActionKind -> GameM ()
performAction iid kind = do
  sid <- fromJustNote "space" <$> investigatorSpace iid
  let after = AfterAction iid kind
  case kind of
    MoveAction -> do
      i <- getInvestigator iid
      vehicles <- fmap catMaybes $ for [c | c <- i.assets, c `notElem` i.lockedAssets] \c -> do
        b <- assetBehavior c
        pure $ (c,) <$> b.moveAction
      -- a spell like Astral Travel is taken instead of the move, so it belongs
      -- among these choices rather than as an action of its own
      spells <- fmap catMaybes $ for [c | c <- i.assets, c `notElem` i.lockedAssets] \c -> do
        b <- assetBehavior c
        pure $ (c,) <$> b.moveBySpell
      spellNames <- for spells \(c, offer) -> (c,offer,) . (.name) <$> getCardDef c
      let normal = [MoveStep (MoveState iid 2 0 2 True False), after]
      if null vehicles && null spells
        then pushAll normal
        else
          chooseFor iid "Move"
            $ label "Move normally" normal
            : [ Choice (CardLabel c) [MarkAssetUsed iid c, MoveStep (MoveState iid steps 0 paid True False), after]
              | (c, (steps, paid)) <- vehicles
              ]
              <> [ Choice
                     (CardsLabel name [c])
                     [ CastSpell
                         iid
                         c
                         [BeginTest (newTest iid skill modifier (SpellTest c) (AfterMoveSpell iid bonus)) {casting = Just c}]
                     , after
                     ]
                 | (c, (skill, modifier, bonus), name) <- spellNames
                 ]
    GatherResourcesAction -> addMoney iid 1 >> push after
    FocusAction -> do
      i <- getInvestigator iid
      let options = [s | s <- allSkills, Map.findWithDefault 0 s i.focus == 0]
      chooseFor
        iid
        "Choose a skill to focus"
        [Choice (SkillLabel s) [FocusSkill iid s False, after] | s <- options]
    WardAction ->
      pushAll [BeginTest (newTest iid Lore 0 (ActionTest WardAction Nothing) (AfterWard iid sid)), after]
    ResearchAction ->
      pushAll
        [ BeginTest (newTest iid Observation 0 (ActionTest ResearchAction Nothing) (AfterResearch iid))
        , after
        ]
    EvadeAction -> do
      ms <- engagedMonsters iid
      mods <- for ms \m -> (.evadeModifier) <$> monsterDef m.card
      i <- getInvestigator iid
      -- Mists of R'lyeh offers lore in place of observation; the monster's evade
      -- modifier applies either way
      alternatives <- fmap catMaybes $ for [c | c <- i.assets, c `notElem` i.lockedAssets] \c ->
        fmap (c,) . (.evadeSkillInstead) <$> assetBehavior c
      let evadeTest skill = newTest iid skill (minimum mods) (ActionTest EvadeAction Nothing) (AfterEvade iid)
      if null alternatives
        then pushAll [BeginTest (evadeTest Observation), after]
        else do
          names <- for alternatives \(c, skill) -> (c,skill,) . (.name) <$> getCardDef c
          chooseFor iid "Choose the skill to test"
            $ label "Test observation" [BeginTest (evadeTest Observation), after]
            : [ Choice
                  (CardsLabel ("Test " <> T.toLower (tshow skill) <> " with " <> name) [c])
                  [CastSpell iid c [BeginTest (evadeTest skill) {casting = Just c}], after]
              | (c, skill, name) <- names
              ]
    AttackAction -> do
      ms <- monstersAt sid
      chooseFor
        iid
        "Choose a monster to attack"
        [Choice (MonsterLabel m.card) [AttackMonster iid m.card, after] | m <- ms]
    TradeAction -> do
      others <- tradePartners iid
      chooseFor
        iid
        "Trade with"
        [Choice (InvestigatorLabel o.id) [TradeWith iid o.id, after] | o <- others]
    ComponentAction ref n ->
      lookupComponentAction iid ref n >>= \case
        Nothing -> push after
        Just a -> do
          let src = case ref of
                SheetRef i -> SourceInvestigator i
                CardRef c -> SourceCard c
                CodexRef a' -> SourceCodex a'
          push after
          a.perform (EffectCtx iid src Nothing)

{- | Feed: "after this monster deals damage to an investigator or ally, it recovers
that much health". Damage an item soaked is not damage an investigator or an ally
took, so it feeds nothing, and neither does horror.
-}
feed :: HarmPlan -> CardId -> GameM ()
feed plan mid = do
  feeds <- hasKeyword Feed mid
  here <- uses #monsters (Map.member mid)
  when (feeds && here) do
    onAlly <- case plan.damageTo of
      Just (cid, k) -> do
        ty <- fmap (.assetType) <$> assetDef cid
        pure (if ty == Just Ally then k else 0)
      Nothing -> pure 0
    let fed = plan.damage - maybe 0 snd plan.damageTo + onAlly
    when (fed > 0) do
      name <- (.name) <$> getCardDef mid
      logText (name <> " feeds, recovering " <> tshow fed <> " health")
      #monsters . ix mid . #damage %= max 0 . subtract fed

-- | What the turn player is asked while choosing what to do with an action.
actionPrompt :: Text
actionPrompt = "Perform an action"

{- | A debug change can add or remove what the turn player may do -- clues make a
research action legal, a defeated monster makes an attack illegal -- so their action
prompt is asked again. It goes behind whatever the change itself set going, so the
options are read after that has resolved.
-}
refreshActionPrompt :: GameM ()
refreshActionPrompt = do
  mturn <- use #turn
  for_ mturn \iid -> do
    pid <- playerOf iid
    asked <- uses #questions (fmap (.prompt) . Map.lookup pid)
    when (asked == Just actionPrompt) $ pushEnd (ActionTurn iid)

enterWith :: MoveState -> SpaceId -> GameM Bool
enterWith ms sid =
  if ms.ignoringMonsters then pure False else noticedEngageOnEntry ms.investigator sid

-- | Walking into a space engages what is there, bar what passes you by.
noticedEngageOnEntry :: InvestigatorId -> SpaceId -> GameM Bool
noticedEngageOnEntry iid = engageOnEntryWhere (fmap not . flip monsterIgnores iid) iid

moveStep :: MoveState -> GameM ()
moveStep ms = do
  i <- getInvestigator ms.investigator
  board <- use #board
  for_ i.space \sid -> do
    adj <- reachable (adjacentSpaces sid board)
    routeSpaces <- reachable (sameRouteSpaces sid board)
    let free = ms.remaining > 0
        paid = not free && ms.paidSteps < ms.maxPaidSteps && i.money >= 1
        stepChoices
          | free = spaceChoices adj \s -> [MoveInvestigator ms {remaining = ms.remaining - 1} s]
          | paid = spaceChoices adj \s -> [PayMoney ms.investigator 1, MoveInvestigator ms {paidSteps = ms.paidSteps + 1} s]
          | otherwise = []
        routes =
          if ms.voluntary && i.money >= 1
            then [Choice (SpaceLabel r) [UseTravelRoute ms r] | r <- routeSpaces]
            else []
        choices = stepChoices <> routes
    unless (null choices)
      $ chooseFor ms.investigator "Move" (choices <> [Choice (DoneLabel "Stop moving") []])

hazardPrompt :: MoveState -> Hazard -> GameM ()
hazardPrompt ms hz = do
  let iid = ms.investigator
  i <- getInvestigator iid
  let payment = case hz of
        HazardDamage -> Just [SufferHarm iid SourceRules NormalHarm 1 0]
        HazardHorror -> Just [SufferHarm iid SourceRules NormalHarm 0 1]
        HazardFocus -> if focusCount i > 0 then Just [] else Nothing
      focusChoices = [Choice (SkillLabel s) [DiscardFocus iid s, MoveStep ms] | (s, n) <- Map.toList i.focus, n > 0]
  case (hz, payment) of
    (HazardFocus, Just _) ->
      chooseFor
        iid
        "Discard a focus to keep moving"
        (focusChoices <> [Choice (DoneLabel "Stop moving") []])
    (_, Just pay) ->
      chooseFor
        iid
        "Pay the hazard to keep moving"
        [label "Pay and keep moving" (pay <> [MoveStep ms]), Choice (DoneLabel "Stop moving") []]
    _ -> pure ()

assignableAssets
  :: InvestigatorId -> HarmKind -> (AssetDef -> Maybe Int) -> (Asset -> Int) -> GameM [(CardId, Int)]
assignableAssets _ DirectHarm _ _ = pure []
assignableAssets iid NormalHarm limit current = do
  i <- getInvestigator iid
  fmap catMaybes $ for i.assets \cid -> do
    md <- assetDef cid
    a <- use (assetL cid)
    pure do
      d <- md
      cap <- limit d
      let room = cap - current a
      guard (room > 0)
      pure (cid, room)

removeInvestigator :: InvestigatorId -> InvestigatorStatus -> Bool -> GameM ()
removeInvestigator iid status addDoom = do
  i <- getInvestigator iid
  when (isPlaying i || i.status == Joining) do
    ms <- engagedMonsters iid
    for_ ms \m -> do
      monsterL m.card . #state .= Exhausted
    for_ i.assets \cid -> discardAsset cid
    investigatorL iid
      %= \inv ->
        inv
          { status = status
          , space = Nothing
          , money = 0
          , clues = 0
          , remnants = 0
          , focus = mempty
          , assets = []
          , delayed = False
          }
    d <- getInvestigatorDef iid
    logText (d.name <> " is " <> tshow status)
    when addDoom $ push (PlaceDoomOnSheet 1)
    t <- use #turn
    ph <- use #phase
    when (t == Just iid) $ case ph of
      ActionPhase -> push (EndActionTurn iid)
      EncounterPhase -> push (EndEncounterTurn iid)
      _ -> pure ()

discardAsset :: CardId -> GameM ()
discardAsset cid = do
  ma <- use (#assets . at cid)
  for_ ma \a -> do
    investigatorL a.owner . #assets %= filter (/= cid)
    #assets . at cid .= Nothing
    attached <- uses #assets (filter ((== Just cid) . (.attachedTo)) . Map.elems)
    for_ attached \x -> discardAsset x.card
    d <- getCardDef cid
    case d.kind of
      AssetCard ad -> case ad.origin of
        AllyDeck -> #decks . #ally %= (<> [cid])
        ItemDeck -> #decks . #item %= (<> [cid])
        SpellDeck -> #decks . #spell %= (<> [cid])
        SpecialPile -> #decks . #special %= (cid :)
        StartingPile -> #decks . #starting %= (cid :)
        ConditionPile -> #decks . #conditions %= (cid :)
        Archive -> #decks . #archive %= (cid :)
      ConditionCard _ -> #decks . #conditions %= (cid :)
      MonsterCard _ -> #decks . #monster %= (cid :)
      _ -> #decks . #removed %= (cid :)

-- 415.4, 415.6
gainCondition :: InvestigatorId -> ConditionName -> GameM ()
gainCondition iid name = do
  already <- hasCondition iid name
  banned <- hasAssetWith iid (elem name . (.bansConditions))
  -- an investigator still joining is being set up, and may start with a condition
  joining <- (== Joining) . (.status) <$> getInvestigator iid
  playing <- (|| joining) <$> investigatorIsPlaying iid
  -- blessed and cursed cancel: "if you would become CURSED, discard this card instead"
  opposing <- case name of
    "BLESSED" -> conditionCard iid "CURSED"
    "CURSED" -> conditionCard iid "BLESSED"
    _ -> pure Nothing
  case opposing of
    _ | banned -> do
      logText (coerce name <> " cannot be held, and is discarded")
      conditionCard iid name >>= traverse_ (push . DiscardAsset)
    Just cid | playing -> do
      logText "The opposing condition is discarded instead"
      push (DiscardAsset cid)
    _ -> when (playing && not already) do
      pile <- use (#decks . #conditions)
      copies <- fmap catMaybes $ for pile \cid ->
        getCardDef cid <&> \d -> case d.kind of
          ConditionCard c
            | c.front.name == name -> Just (cid, False)
            | c.backIsCondition && c.back.name == name -> Just (cid, True)
          _ -> Nothing
      pickRandom copies >>= \case
        Nothing -> logText ("No copies of " <> coerce name <> " remain")
        Just (cid, flipped) -> do
          removeCardEverywhere cid
          #assets
            . at cid
            ?= Asset
              { card = cid
              , owner = iid
              , damage = 0
              , horror = 0
              , flipped = flipped
              , attachedTo = Nothing
              , tokens = mempty
              }
          investigatorL iid . #assets %= (<> [cid])

-- 435.8, 435.9
checkFocusLimit :: InvestigatorId -> Bool -> GameM ()
checkFocusLimit iid evenIfExceeds = unless evenIfExceeds do
  i <- getInvestigator iid
  limit <- focusLimit iid
  for_ limit \l ->
    when (focusCount i > l)
      $ chooseFor
        iid
        "Discard a focus token"
        [Choice (SkillLabel s) [DiscardFocus iid s] | (s, n) <- Map.toList i.focus, n > 0]

refillDisplay :: GameM ()
refillDisplay = do
  display <- use (#decks . #display)
  deck <- use (#decks . #item)
  let need = 5 - length display
      (new, rest) = splitAt need deck
  when (need > 0) do
    #decks . #display .= display <> new
    #decks . #item .= rest

buyPrompt :: EffectCtx -> Maybe Trait -> Bool -> Maybe Int -> Effect -> Int -> GameM ()
buyPrompt ctx mtrait half limit ifBought bought = do
  let iid = ctx.investigator
      finish = if bought > 0 then [ResolveEffect ctx ifBought] else [CycleDisplay iid 2]
  markup <- displayMarkup iid
  display <- use (#decks . #display)
  i <- getInvestigator iid
  priced <- fmap catMaybes $ for display \cid -> do
    ok <- maybe (pure True) (\t -> cardMatches (WithTrait t) cid) mtrait
    md <- assetDef cid
    pure do
      d <- md
      v <- (+ markup) <$> d.value
      guard ok
      pure (cid, if half then (v + 1) `div` 2 else v)
  -- a card that halves a price (Fine Clothes, Henry Wan) says it does not stack,
  -- so it is offered only on a purchase that is not halved already
  discounts <- if half then pure [] else halfPriceCards iid
  let options = [o | o@(_, price) <- priced, price <= i.money]
      more = BuyFromDisplayMore ctx mtrait half limit ifBought (bought + 1)
      halved price = (price + 1) `div` 2
  if maybe False (bought >=) limit
    then pushAll finish
    else
      chooseFor iid "Buy from the display"
        $ Choice (DoneLabel "Done") finish
        : [Choice (CardLabel cid) [BuyCard iid cid price, more] | (cid, price) <- options]
          <> [ Choice
                 (CardsLabel (name <> ": half price") [dcid, cid])
                 [MarkAssetUsed iid dcid, BuyCard iid cid (halved price), more]
             | (dcid, name) <- discounts
             , (cid, price) <- priced
             , halved price <= i.money
             ]

displayMarkup :: InvestigatorId -> GameM Int
displayMarkup iid = do
  mr <- use #rumor
  ignored <- elem iid <$> use #rumorIgnored
  codes <- traverse (cardCode . (.card)) mr
  pure $ if codes == Just "truckers-strike-leads-to-shortages" && not ignored then 2 else 0

addToCodex :: ArchiveNumber -> Bool -> GameM ()
addToCodex n flipped = do
  archive <- use (#decks . #archive)
  matches <-
    filterM
      (\cid -> getCardDef cid <&> \d -> case d.kind of ArchiveCard a -> a.number == n; _ -> False)
      archive
  case matches of
    (cid : _) -> do
      removeCardEverywhere cid
      let entry = CodexEntry {number = n, card = cid, flipped = flipped, tokens = mempty, fired = []}
      #codex %= (<> [entry])
      logText
        ("Card " <> tshow (coerce n :: Int) <> " added to the codex" <> (if flipped then " facedown" else ""))
      push CheckStateTriggers
      (codexBehavior n).onAdd entry
    [] -> logText ("Archive card unavailable: " <> tshow (coerce n :: Int))

{- | Cards a starting possession's picture may be taken from. The investigator's
own copy of a deck card does not exist until it is taken, so for the choice
buttons a deck's copy stands in; nothing is moved by looking.
-}
startingPool :: GameM [CardId]
startingPool = do
  d <- use #decks
  pure (d.starting <> d.spell <> d.item <> d.ally <> d.special)

possessionsText :: [StartingPossession] -> Text
possessionsText [] = "Nothing"
possessionsText ps = T.intercalate ", " (map one ps)
 where
  one = \case
    StartingCard code -> maybe (coerce code) (.name) (cardDef code)
    StartingMoney n -> "$" <> tshow n
    StartingRemnants n -> tshow n <> " remnants"
    StartingClues n -> tshow n <> " clues"
    StartingCondition c -> coerce c
    StartingChoice os -> T.intercalate " or " (map possessionsText os)

gainFromDeck :: InvestigatorId -> AssetDeckKind -> Maybe Trait -> Maybe ValueBound -> GameM ()
gainFromDeck iid kind mtrait mbound = do
  deck <- use (assetDeckLens kind)
  matching <- for deck (itemMatches mtrait mbound)
  let (skipped, rest) = break snd (zip deck matching)
  case rest of
    [] -> pure ()
    ((cid, _) : after) -> do
      back <- shuffle (map fst skipped)
      assetDeckLens kind .= map fst after <> back
      pushAll [GainAsset iid cid, AfterGainedFromDeck iid cid]

buyRevealed :: EffectCtx -> AssetDeckKind -> [CardId] -> Maybe Int -> Pricing -> Int -> GameM ()
buyRevealed ctx kind revealed limit pricing bought = do
  let iid = ctx.investigator
      finish = [ReturnToBottom kind revealed]
  i <- getInvestigator iid
  options <- fmap catMaybes $ for revealed \cid -> do
    mv <- cardValue cid
    pure do
      v <- mv
      let price = case pricing of
            FullPrice -> v
            HalfPrice -> (v + 1) `div` 2
            FlatPrice flat -> flat
      guard (price <= i.money)
      pure (cid, price)
  if maybe False (bought >=) limit || null revealed
    then pushAll finish
    else
      chooseFor iid "Buy from the revealed cards"
        $ Choice (DoneLabel "Done") finish
        : [ Choice
              (CardLabel cid)
              [ BuyCard iid cid price
              , AfterGainedFromDeck iid cid
              , BuyRevealed ctx kind (filter (/= cid) revealed) limit pricing (bought + 1)
              ]
          | (cid, price) <- options
          ]

tradePrompt :: InvestigatorId -> InvestigatorId -> GameM ()
tradePrompt iid other = do
  a <- getInvestigator iid
  b <- getInvestigator other
  let tradable i =
        filterM
          (\cid -> cardMatches ItemCard cid ||^ cardMatches AllyCard cid ||^ cardMatches SpellCard cid)
          i.assets
  aCards <- tradable a
  bCards <- tradable b
  let give giver receiver i =
        [label "Give $1" [TradeTransfer giver receiver (TradeMoney 1), TradeWith iid other] | i.money > 0]
          <> [ label "Give 1 clue" [TradeTransfer giver receiver (TradeClues 1), TradeWith iid other] | i.clues > 0
             ]
          <> [ label "Give 1 remnant" [TradeTransfer giver receiver (TradeRemnants 1), TradeWith iid other]
             | i.remnants > 0
             ]
  chooseFor iid "Trade"
    $ [Choice (DoneLabel "Done trading") []]
    <> give iid other a
    <> map (\c -> c & #label .~ TextLabel ("Take: " <> labelText c.label)) (give other iid b)
    <> [Choice (CardLabel c) [TradeTransfer iid other (TradeCard c), TradeWith iid other] | c <- aCards]
    <> [Choice (CardLabel c) [TradeTransfer other iid (TradeCard c), TradeWith iid other] | c <- bCards]
 where
  (||^) x y = x >>= \r -> if r then pure True else y
  labelText = \case
    TextLabel t -> t
    other' -> tshow other'

mythosTokenName :: MythosToken -> Text
mythosTokenName = \case
  SpreadDoomToken -> "Spread doom"
  SpawnMonsterToken -> "Spawn monster"
  ReadHeadlineToken -> "Read headline"
  SpawnClueToken -> "Spawn clue"
  GateBurstToken -> "Gate burst"
  ReckoningToken -> "Reckoning"
  BlankToken -> "Blank"
  SpreadTerrorToken -> "Spread terror"

reckoningSources :: GameM [Source]
reckoningSources = do
  codex <- use #codex
  rumorNow <- use #rumor
  invs <- playingInvestigators
  assetSources <- fmap concat $ for invs \i -> fmap catMaybes $ for i.assets \cid -> do
    b <- assetBehavior cid
    pure $ SourceCard cid <$ b.reckoning
  pure
    $ [SourceScenario]
    <> [SourceCodex e.number | e <- codex, isJust ((codexBehavior e.number).reckoning e)]
    <> [SourceHeadline r.card | Just r <- [rumorNow]]
    <> assetSources

resolveReckoning :: Source -> GameM ()
resolveReckoning src = do
  l <- leaderPlayer
  leaderInv <- fromMaybe (error "no leader investigator") <$> investigatorOfPlayer l
  case src of
    SourceScenario -> do
      sc <- getScenarioDef
      push (ResolveEffect (EffectCtx leaderInv src Nothing) sc.reckoning)
    SourceCodex n ->
      codexEntry n >>= traverse_ \entry ->
        for_ ((codexBehavior n).reckoning entry) \e -> push (ResolveEffect (EffectCtx leaderInv src Nothing) e)
    SourceCard cid -> do
      b <- assetBehavior cid
      owner <- (.owner) <$> use (assetL cid)
      for_ b.reckoning \e -> push (ResolveEffect (EffectCtx owner src Nothing) e)
    SourceHeadline cid -> do
      d <- getCardDef cid
      case d.kind of
        HeadlineCard h -> for_ h.reckoning \e -> push (ResolveEffect (EffectCtx leaderInv src Nothing) e)
        _ -> pure ()
    _ -> pure ()

withEventDeck :: ([CardId] -> GameM ()) -> GameM ()
withEventDeck f = do
  deck <- use (#decks . #event)
  if null deck
    then do
      push (PlaceDoomOnSheet 1)
      discard <- use (#decks . #eventDiscard)
      shuffled <- shuffle discard
      #decks . #event .= shuffled
      #decks . #eventDiscard .= []
    else f deck

-- 406.3a, 461.1a, terror (Under Dark Waves)
checkDoomThresholds :: SpaceId -> GameM ()
checkDoomThresholds sid = do
  s <- getSpace sid
  for_ s.neighborhood \nid -> do
    anomalies <- codexHas 2
    outbreak <- codexHas 1
    terror <- codexHas 61
    board <- use #board
    n <- getNeighborhood nid
    let total = neighborhoodDoom nid board
    when (anomalies && not n.anomaly && (s.doom >= 3 || total >= 5)) do
      neighborhoodL nid . #anomaly .= True
      logText ("An anomaly opens in " <> n.name)
    when (outbreak && s.doom >= 4) do
      spaceL sid . #doom -= 3
      let others = filter (/= sid) (neighborhoodSpaces nid board)
      logText ("Outbreak in " <> n.name)
      pushAll [PlaceDoomInOrder SourceRules others, PlaceDoomOnSheet 1]
    when (terror && total >= 6) do
      let spaces =
            [x | x <- neighborhoodSpaces nid board, maybe False ((> 0) . (.doom)) (Map.lookup x board.spaces)]
      chooseGroup
        "Remove all doom from a space"
        (spaceChoices spaces \x -> [ClearSpaceDoom x, PlaceDoomOnSheet 1, SpreadTerror nid])

enterPhase :: Phase -> GameM ()
enterPhase p = do
  #phase .= p
  #phasesEntered %= (<> [p])

checkStateTriggers :: GameM ()
checkStateTriggers = do
  entries <- use #codex
  candidates <- fmap concat $ for entries \e ->
    pure [(e, t) | t <- (codexBehavior e.number).triggers, not (t.once && t.key `elem` e.fired)]
  firing <- findM (\(e, t) -> t.condition e) candidates
  for_ firing \(e, t) -> do
    when t.once
      $ #codex
      %= map (\x -> if x.number == e.number then x {fired = x.fired <> [t.key]} else x)
    push CheckStateTriggers
    t.action e
 where
  findM _ [] = pure Nothing
  findM p (x : xs) = p x >>= \ok -> if ok then pure (Just x) else findM p xs

resolveEncounter :: InvestigatorId -> EncounterDeck -> GameM ()
resolveEncounter iid deck = do
  sid <- fromJustNote "space" <$> investigatorSpace iid
  s <- getSpace sid
  mcid <- case deck of
    TerrorDeck nid -> do
      n <- getNeighborhood nid
      pickRandom n.attachedTerror
    _ -> listToMaybe <$> use (deckLens deck)
  for_ mcid \cid -> do
    case deck of
      TerrorDeck nid -> neighborhoodL nid . #attachedTerror %= filter (/= cid)
      _ -> deckLens deck %= drop 1
    #activeCard ?= cid
    #encounter
      ?= EncounterState
        { investigator = iid
        , card = cid
        , deck = deck
        , gainedNeighborhoodClue = False
        , returnToArchive = False
        , section = Nothing
        }
    d <- getCardDef cid
    board <- use #board
    let ctx = EffectCtx {investigator = iid, source = SourceEncounter cid, testResult = Nothing}
        nbhdDoom = maybe 0 (`neighborhoodTerror'` board) s.neighborhood
        inRange v ((lo, mhi), _) = v >= lo && maybe True (v <=) mhi
        found = case d.kind of
          NeighborhoodCard _ es -> Map.lookup sid es
          EventCard e -> Map.lookup sid e.encounters
          StreetCard es -> case s.kind of StreetSpace st -> Map.lookup st es; _ -> Nothing
          TravelRouteCard es -> case s.kind of TravelRouteSpace r -> Map.lookup r es; _ -> Nothing
          ThresholdCard es -> case s.kind of ThresholdSpace t -> Map.lookup t es; _ -> Nothing
          AnomalyCard a -> snd <$> find (inRange s.doom) a.byDoom
          TerrorCard t -> snd <$> find (inRange nbhdDoom) t.byTerror
          MysteryCard m ->
            Just
              (Encounter m.opening.text (Seq [m.opening.effect, Choose [(t, e.effect) | (t, e) <- m.branches]]))
          _ -> Nothing
        ranged xs v = (,length xs) <$> findIndex (inRange v) xs
    #encounter . _Just . #section .= case d.kind of
      AnomalyCard a -> ranged a.byDoom s.doom
      TerrorCard t -> ranged t.byTerror nbhdDoom
      -- street cards print Residential, Bridge, Scenic top to bottom, the StreetType order
      StreetCard _ | StreetSpace st <- s.kind -> Just (fromEnum st, length [minBound .. maxBound :: StreetType])
      _ -> Nothing
    case found of
      Just enc -> do
        unless (T.null enc.text) $ logText enc.text
        pushAll [ResolveEffect ctx enc.effect, AcknowledgeEncounter, FinishEncounter]
      Nothing -> do
        logText ("No encounter for this space on " <> d.name)
        pushAll [AcknowledgeEncounter, FinishEncounter]
 where
  find p = listToMaybe . filter p
  neighborhoodTerror' nid board = maybe 0 (.terror) (Map.lookup nid board.neighborhoods)

-- | Deal the top card of a pile straight into play, for testing.
debugDrawDeck :: InvestigatorId -> DebugDeck -> GameM ()
debugDrawDeck iid = \case
  DeckMonster -> push (SpawnMonsterAt Nothing False)
  DeckHeadline -> push (DrawHeadline iid)
  DeckStreet -> push (ResolveEncounterFrom iid StreetDeck)
  DeckThreshold -> push (ResolveEncounterFrom iid ThresholdDeck)
  DeckTravelRoute -> push (ResolveEncounterFrom iid TravelRouteDeck)
  DeckAnomaly -> push (ResolveEncounterFrom iid AnomalyDeck)
  DeckNeighborhood nid -> push (ResolveEncounterFrom iid (NeighborhoodDeck nid))
  d -> do
    let l :: Lens' Game [CardId]
        l = debugDeckLens d
    use l >>= \case
      [] -> logText "Debug: that pile is empty"
      (cid : rest) -> l .= rest >> pushAll [GainAsset iid cid, AfterGainedFromDeck iid cid]

debugDeckLens :: DebugDeck -> Lens' Game [CardId]
debugDeckLens = \case
  DeckItem -> #decks . #item
  DeckAlly -> #decks . #ally
  DeckSpell -> #decks . #spell
  DeckSpecial -> #decks . #special
  DeckStarting -> #decks . #starting
  DeckCondition -> #decks . #conditions
  DeckMonster -> #decks . #monster
  DeckHeadline -> #decks . #headline
  DeckStreet -> #decks . #street
  DeckThreshold -> #decks . #threshold
  DeckTravelRoute -> #decks . #travelRoute
  DeckAnomaly -> #decks . #anomaly
  DeckNeighborhood nid -> #decks . #neighborhoods . at nid . non []

deckLens :: EncounterDeck -> Lens' Game [CardId]
deckLens = \case
  NeighborhoodDeck nid -> #decks . #neighborhoods . at nid . non []
  StreetDeck -> #decks . #street
  TravelRouteDeck -> #decks . #travelRoute
  ThresholdDeck -> #decks . #threshold
  MysteryDeck sid -> #decks . #mysteries . at sid . non []
  AnomalyDeck -> #decks . #anomaly
  TerrorDeck _ -> #decks . #terror

-- 426.11, 430.7
finishEncounter :: GameM ()
finishEncounter = do
  menc <- use #encounter
  #encounter .= Nothing
  for_ menc \enc -> #activeCard %= \cur -> if cur == Just enc.card then Nothing else cur
  for_ menc \enc -> do
    d <- getCardDef enc.card
    if enc.returnToArchive
      then #decks . #archive %= (enc.card :)
      else case (d.kind, enc.deck) of
        (EventCard e, _)
          | enc.gainedNeighborhoodClue -> #decks . #eventDiscard %= (enc.card :)
          | otherwise -> do
              let l :: Lens' Game [CardId]
                  l = #decks . #neighborhoods . at e.neighborhood . non []
              deck <- use l
              l <~ shuffleIntoTopTwo enc.card deck
        (_, TerrorDeck _) -> #decks . #terror %= (<> [enc.card])
        (_, deck) -> deckLens deck %= (<> [enc.card])

setupScenarioDecks :: GameM ()
setupScenarioDecks = do
  sc <- getScenarioDef
  for_ sc.anomalySet \setName -> do
    cards <- uses #decks (.setAside)
    anomalies <-
      filterM
        (\cid -> getCardDef cid <&> \d -> case d.kind of AnomalyCard a -> a.set == setName; _ -> False)
        cards
    shuffled <- shuffle anomalies
    #decks . #setAside %= filter (`notElem` anomalies)
    #decks . #anomaly .= shuffled
  for_ sc.terrorSet \setName -> do
    cards <- uses #decks (.setAside)
    terrors <-
      filterM
        (\cid -> getCardDef cid <&> \d -> case d.kind of TerrorCard t -> t.set == setName; _ -> False)
        cards
    shuffled <- shuffle terrors
    #decks . #setAside %= filter (`notElem` terrors)
    #decks . #terror .= shuffled

runDebug :: DebugAction -> GameM ()
runDebug = \case
  DebugSetMoney iid n -> investigatorL iid . #money .= n
  DebugSetClues iid n -> investigatorL iid . #clues .= n
  DebugSetRemnants iid n -> investigatorL iid . #remnants .= n
  DebugSetDamage iid n -> investigatorL iid . #damage .= n >> push (CheckDefeat iid)
  DebugSetHorror iid n -> investigatorL iid . #horror .= n >> push (CheckDefeat iid)
  DebugMoveInvestigator iid sid -> investigatorL iid . #space ?= sid
  DebugSetSpaceDoom sid n -> spaceL sid . #doom .= n >> push CheckStateTriggers
  DebugSetSheetDoom n -> #sheetDoom .= n >> push CheckStateTriggers
  DebugSetSheetClues n -> #sheetClues .= n >> push CheckStateTriggers
  DebugSetSheetMarkers n -> #sheetMarkers .= n >> push CheckStateTriggers
  DebugGainFromDisplay iid cid -> do
    inDisplay <- uses (#decks . #display) (elem cid)
    when inDisplay $ push (GainFromDisplay iid cid)
  DebugDiscardCard cid -> discardAsset cid
  DebugAddToCodex n -> push (AddArchiveToCodex n)
  DebugResolveEffect iid eff -> push (ResolveEffect (EffectCtx iid SourceDebug Nothing) eff)
  DebugDrawMythos iid tok -> do
    pid <- playerOf iid
    push (ResolveMythosToken pid tok)
  DebugSetDelayed iid b -> investigatorL iid . #delayed .= b
  -- through the usual path, so the opposing condition and any ban still apply
  DebugGainCondition iid name -> push (GainConditionMsg iid name)
  DebugSetFocus iid skill n ->
    investigatorL iid . #focus . at skill .= (if n > 0 then Just n else Nothing)
  DebugDrawDeck iid deck -> debugDrawDeck iid deck
  DebugDrawCard iid deck cid -> do
    -- put the wanted card where this deck deals from, then deal it normally
    debugDeckLens deck %= case deck of
      DeckMonster -> \cs -> filter (/= cid) cs <> [cid]
      _ -> \cs -> cid : filter (/= cid) cs
    debugDrawDeck iid deck
  DebugSetMonsterDamage mid n -> #monsters . ix mid . #damage .= max 0 n
  DebugDefeatMonster mid -> do
    present <- uses #monsters (Map.member mid)
    when present $ push (DefeatMonster mid SourceDebug)
  DebugSetDice values -> #test . _Just . #dice .= [Die v False | v <- values]
  DebugSetAddedSuccesses n -> #test . _Just . #addedSuccesses .= n
