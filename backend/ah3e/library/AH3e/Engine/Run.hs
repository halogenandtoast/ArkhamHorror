module AH3e.Engine.Run (runMessage) where

import AH3e.Content
import AH3e.Engine.Behavior
import AH3e.Engine.Effect
import AH3e.Engine.Helpers
import AH3e.Engine.Hooks
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Engine.Setup (
  availableScenarios,
  buildBoard,
  moveCornerTile,
  setupScenario,
  turnThresholdTile,
 )
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
import Data.Set qualified as Set
import Data.Text qualified as T

{- | A card can leave a message for the test it belongs to queued behind that test's
own ending -- Chef's Knife's raise, a reroll a reaction bought -- and by the time it
runs there is nothing left to act on. Rather than let each of them fail on a test
that has gone, the ones that only speak to a test in progress are skipped.
-}
runMessage :: Message -> GameM ()
runMessage msg = do
  live <- uses #test isJust
  gone <- case namesAMonsterOnTheBoard msg of
    Just mid -> uses #monsters (not . Map.member mid)
    Nothing -> pure False
  unless (gone || (actsOnTestInProgress msg && not live)) (dispatch msg)

{- | A monster a queued message names can be defeated before that message runs -- the
monster phase lines up a ready step for every exhausted monster at once, and a card
answering the first can finish off a later one -- and the handler then has nothing to
look up. Only the messages that read a monster already on the board are listed: the
ones that put one there must still run.
-}
namesAMonsterOnTheBoard :: Message -> Maybe CardId
namesAMonsterOnTheBoard = \case
  ReadyMonster mid -> Just mid
  DisengageMonster _ mid -> Just mid
  MonsterStep mid _ _ -> Just mid
  AttackMonster _ mid -> Just mid
  CheckEngagement mid -> Just mid
  _ -> Nothing

actsOnTestInProgress :: Message -> Bool
actsOnTestInProgress = \case
  RollDice -> True
  ContinueTest -> True
  FinishTest -> True
  ToggleTestAsset _ -> True
  SetTestSkill _ -> True
  SpendForReroll _ -> True
  RerollDie _ _ -> True
  RerollUpTo _ _ -> True
  RerollUpToNow _ _ -> True
  RerollOneOf {} -> True
  RerollAll _ -> True
  RaiseInsteadOfReroll {} -> True
  RollAdditionalDice _ _ -> True
  RollADiePerFailure _ -> True
  RemoveADie _ -> True
  RemoveDieAt _ -> True
  AddToDie _ -> True
  ChooseDieToSet _ -> True
  ChooseDieResult -> True
  SetDieValue _ _ -> True
  RaiseDie _ -> True
  MarkUsedInTest _ -> True
  {- Successes and riders are not among them: both are banked for the next test to
  begin when there is none in progress, which is how a spell's cast cost reaches the
  test it pays for. -}
  _ -> False

dispatch :: Message -> GameM ()
dispatch msg = case msg of
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
    StartingEffect _ eff -> push (ResolveEffect (EffectCtx iid (SourceInvestigator iid) Nothing) eff)
    {- The pictures go on the button by card code: the investigator's own copy does
    not exist until it is taken, and a box that is not on the table has dealt no
    copy to borrow one from. -}
    StartingChoice options ->
      chooseFor
        iid
        "Choose a starting possession"
        [ Choice
            (CardCodesLabel (possessionsText o) [code | StartingCard code <- o])
            [GainStartingPossessions iid o]
        | o <- options
        ]
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
    here <- startingSpaceOnBoard
    for_ here \sid -> investigatorL iid . #space ?= sid
    investigatorL iid . #status .= Playing
  FinalPreparations -> do
    sc <- getScenarioDef
    pushAll
      $ [SpawnClue, SpawnClue, SpawnClue]
      {- The sheet prints a space for each of these, so there is nothing for anyone to
      decide: they go down as printed rather than asking the table to order them. -}
      <> map (PlaceDoom SourceRules) sc.startingDoom
      <> [SpreadDoom]
      -- the ally deck is shuffled by now, so a bystander is whoever comes off the top
      <> map PlaceBystander sc.startingBystanders
      <> map AddArchiveToCodex sc.codex
      <> [SetupEncounterDecks, BeginRound]
  SetupEncounterDecks -> setupScenarioDecks
  -- Round structure (rule 200)
  BeginRound -> do
    #round += 1
    #rumorIgnored .= []
    #activatedMonsters .= []
    #terrorEncountered .= []
    #investigators %= Map.map \i ->
      i {performed = [], usedAssets = [], usedAbilities = [], spacesMovedThisRound = 0}
    r <- use #round
    logText ("Round " <> tshow r)
    push BeginActionPhase
  BeginActionPhase -> do
    enterPhase ActionPhase
    start <- startingSpaceOnBoard
    joining <- uses #investigators (filter ((== Joining) . (.status)) . Map.elems)
    for_ ((,) <$> joining <*> maybeToList start) \(i, sid) -> do
      investigatorL i.id . #space ?= sid
      investigatorL i.id . #status .= Playing
      void (noticedEngageOnEntry i.id sid)
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
    pushAll [CheckReactions (AtStartOfTurn iid) [], ActionTurn iid]
  ActionTurn iid -> do
    i <- getInvestigator iid
    if not (isPlaying i)
      then push (EndActionTurn iid)
      else do
        allowance <- actionAllowance iid
        -- a free ability is still on offer once both actions are spent
        free <- freeActionsFor iid
        {- A turn ends once, even when a card hands out another action and the turn
        comes back round to here (DRIVEN); whatever answers the ending has had its
        say by then. -}
        wrapped <- usedAbility iid "end-of-turn"
        -- what a card takes as the turn closes, behind whatever was offered
        closing <- if wrapped then pure [] else atEndOfOwnerTurnFor iid
        let wrapUp =
              [MarkAbilityUsed iid "end-of-turn" | not wrapped]
                <> [CheckReactions (AtEndOfTurn iid) [] | not wrapped]
                <> closing
                <> [EndActionTurn iid]
            endTurn = Choice (DoneLabel "End turn") wrapUp
            freeChoices =
              [Choice (TextLabel a.label) [PerformFreeAction iid ref n] | (ref, n, a) <- free]
        if i.actionsTaken >= allowance
          then
            if null freeChoices
              then pushAll wrapUp
              else chooseFor iid actionPrompt (freeChoices <> [endTurn])
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
                  ( [Choice (actionLabel a) [PerformAction iid a] | a <- actions]
                      <> freeChoices
                      <> [endTurn]
                  )
  StandUp iid -> do
    investigatorL iid . #delayed .= False
    d <- getInvestigatorDef iid
    logText (d.name <> " stands up")
    investigatorL iid . #actionsTaken += 1
    push (ActionTurn iid)
  {- A granted action is performed as its taker's own -- the restrictions on what they
  may do still hold -- but it costs them none of their own actions. Repeating one they
  have already taken is what Witch Blood allows, so that skips the used-up check. -}
  PerformGrantedAction iid kind again -> do
    legal <- legalActions iid
    done <- (.performed) <$> getInvestigator iid
    when (if again then kind `elem` done else kind `elem` legal) do
      unless again $ investigatorL iid . #performed %= (<> [kind])
      performAction iid kind
  {- 'legalActions' leaves out what they have already done this round, which is
  the one rule this action lifts, so the list is taken with their record set aside. -}
  GrantAnotherAction iid -> do
    i <- getInvestigator iid
    investigatorL iid . #performed .= []
    options <- legalActions iid
    investigatorL iid . #performed .= i.performed
    components <- componentActionsFor iid
    let actionLabel = \case
          ComponentAction ref n
            | (name : _) <- [def.label | (r, k, def) <- components, r == ref, k == n] ->
                TextLabel name
          a -> ActionLabel a
    chooseFor
      iid
      "Take an additional action"
      [Choice (actionLabel k) [PerformGrantedAction iid k (k `elem` i.performed)] | k <- options]
  OfferGrantedAction from kind -> do
    others <- filter ((/= from) . (.id)) <$> playingInvestigators
    takers <- filterM (fmap (elem kind) . legalActions . (.id)) others
    unless (null takers)
      $ chooseGroup
        "Who takes the granted action?"
        ( Choice (DoneLabel "Nobody") []
            : [Choice (InvestigatorLabel o.id) [PerformGrantedAction o.id kind False] | o <- takers]
        )
  PerformAction iid kind -> do
    legal <- legalActions iid
    unless (kind `elem` legal) $ error ("illegal action " <> show kind)
    investigatorL iid . #performed %= (<> [kind])
    investigatorL iid . #actionsTaken += 1
    when (kind == MoveAction) $ investigatorL iid . #spacesMoved .= 0
    performAction iid kind
  PerformFreeAction iid ref n -> do
    free <- freeActionsFor iid
    case [a | (r, k, a) <- free, r == ref, k == n] of
      (a : _) -> do
        -- a push goes to the front, so the turn is queued first and the ability's
        -- own messages land ahead of it; otherwise the next prompt is built before
        -- anything the ability queued has run
        push (ActionTurn iid)
        a.perform (EffectCtx iid (refSource ref) Nothing)
      [] -> push (ActionTurn iid)
  AfterAction iid kind -> do
    t <- use #turn
    ph <- use #phase
    moved <- (.spacesMoved) <$> getInvestigator iid
    others <- filter ((/= iid) . (.id)) <$> playingInvestigators
    fromCards <- afterOwnerActionFor iid kind
    pushAll
      $ fromCards
      <> [CheckReactions (AfterGatherResources iid) [] | kind == GatherResourcesAction]
      <> [CheckReactions (AfterResearchAction iid) [] | kind == ResearchAction]
      <> [CheckReactions (AfterMoveAction iid) [] | kind == MoveAction]
      <> [CheckReactions (AfterMoveDistance iid moved) [] | kind == MoveAction]
      <> [CheckReactions (AfterAnyAction iid kind) []]
      <> [CheckReactions (AnotherPerformsAction o.id iid kind) [] | o <- others]
      <> [MonstersWatchAction iid kind]
      <> [ActionTurn iid | t == Just iid, ph == ActionPhase]
  {- A card may answer the end of a turn by handing out another action (DRIVEN), and
  the turn then comes back round to here; the second pass finds it already over and
  leaves the earlier one to stand. -}
  EndActionTurn iid -> do
    turn <- use #turn
    when (turn == Just iid) do
      investigatorL iid . #active .= False
      #turn .= Nothing
      push NextActionTurn
  -- Monster phase (rule 202)
  BeginMonsterPhase -> do
    enterPhase MonsterPhase
    #activatedMonsters .= []
    #monsters %= Map.map (\m -> m {prey = Nothing})
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
  {- A card may answer a monster's activation with something else (Lure Monster),
  so the activation itself waits behind that offer. The monster counts as having
  activated either way. -}
  ActivateMonster mid -> do
    #activatedMonsters %= (<> [mid])
    offers <- activationReplacements mid
    if null offers
      then push (DoActivateMonster mid)
      else
        chooseGroup "Answer this monster's activation?"
          $ Choice (DoneLabel "Let it activate") [DoActivateMonster mid]
          : [Choice (TextLabel r.label) r.messages | r <- offers]
  DoActivateMonster mid -> do
    ready <- isMonsterReady mid
    when ready do
      d <- monsterDef mid
      named <- activationPrey mid
      case d.activation of
        Hunter rule -> push (MonsterStep mid d.speed (TowardPrey (fromMaybe rule named)))
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
      {- A card may put something on the board that every monster makes for as
      though it were an investigator, so it joins whatever the activation names. -}
      quarry <- codexQuarrySpaces
      hunted <- case target of
        TowardSpaces rule -> ruleSpaces (Just mid) rule
        TowardPrey rule ->
          -- a codex card may send every hunter after something else entirely
          codexPreyInstead mid >>= \case
            Just sids -> pure sids
            Nothing -> do
              prey <- ruleInvestigators rule
              noticed <- filterM (fmap not . monsterIgnores mid . (.id)) prey
              pure (mapMaybe (.space) noticed)
      let targets = nub (hunted <> quarry)
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
    holds <- monsterHoldsItsQuarry mid
    codex <- codexMonsterArrived mid sid
    -- whether it walked in or spawned there, it has arrived (One Man Army)
    arrivedFor <- investigatorsAt sid
    pushAll (codex <> [CheckReactions (AfterMonsterArrives i.id mid) [] | i <- arrivedFor])
    when (ready && not holds) do
      here <- investigatorsAt sid
      present <- filterM (fmap not . monsterIgnores mid . (.id)) here
      prey <- activationPrey mid
      engageTargets mid present prey >>= \case
        Right is -> for_ is \i ->
          engagementReplacement mid i.id >>= \case
            Just instead -> pushAll instead
            Nothing -> engage i.id mid
        Left pool ->
          chooseGroup
            "Choose the investigator the monster engages"
            [Choice (InvestigatorLabel i.id) [EngageMonster i.id mid] | i <- pool]
  MonsterAttackStep [] -> push MonsterReadyStep
  MonsterAttackStep (iid : rest) -> do
    ms <- map (.card) <$> engagedMonsters iid
    stoppers <- attackStoppers iid
    case (ms, stoppers) of
      (_ : _, (cid, cardName) : _) ->
        chooseFor
          iid
          (cardName <> ": call off the attack?")
          [ Choice (DoneLabel "Let them attack") [MonstersAttack iid ms, MonsterAttackStep rest]
          , Choice
              (TextLabel ("Discard " <> cardName <> " to disengage and exhaust them"))
              ( DiscardAsset cid
                  : concat [[DisengageMonster iid m, ExhaustMonster m] | m <- ms]
                    <> [MonsterAttackStep rest]
              )
          ]
      _ -> pushAll [MonstersAttack iid ms, MonsterAttackStep rest]
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
      b <- monsterBehavior mid
      own <- b.afterAttack mid iid
      pushAll (SufferHarm iid (SourceMonster mid) NormalHarm d.damage d.horror : own)
  MonsterReadyStep -> do
    ms <- uses #monsters (filter ((== Exhausted) . (.state)) . Map.elems)
    everyone <- playingInvestigators
    closing <- codexEndOfMonsterPhase
    pushAll
      $ [ReadyMonster m.card | m <- ms]
      <> closing
      <> [CheckReactions (AtEndOfMonsterPhase i.id) [] | i <- everyone]
      <> [BeginEncounterPhase]
  ReadyMonster mid -> do
    m <- getMonster mid
    may <- monsterCanReady mid
    when may do
      setMonsterState mid Ready
      push (MonsterEngagesIn mid m.space)
  ExhaustMonster mid -> do
    ok <- canBeExhausted mid
    when ok do
      setMonsterState mid Exhausted
      monsterBehavior mid >>= \b -> b.afterExhausted mid >>= pushAll
  {- Someone standing beside the one a monster picks may take the engagement instead
  (Tommy Muldoon, Mr. Pawterson's neighbour), so the engagement itself waits behind
  that offer. -}
  EngageMonster iid mid -> do
    shields <- shieldsFor iid
    case shields of
      [] -> push (EngageMonsterNow iid mid)
      (guardian : _) -> do
        monsterName <- (.name) <$> getCardDef mid
        chooseFor
          guardian
          ("Take that engagement with " <> monsterName <> " instead?")
          [ Choice (DoneLabel "No") [EngageMonsterNow iid mid]
          , Choice (TextLabel "Engage me instead") [EngageMonsterNow guardian mid]
          ]
  EngageMonsterNow iid mid ->
    engagementReplacement mid iid >>= \case
      Just instead -> pushAll instead
      Nothing -> engage iid mid
  SetMonsterPrey mid iid -> do
    monsterL mid . #prey ?= iid
    name <- (.name) <$> getCardDef mid
    who <- (.name) <$> getInvestigatorDef iid
    logText (name <> " sets its sights on " <> who)
  DisengageMonster iid mid -> do
    holds <- monsterHoldsItsQuarry mid
    m <- getMonster mid
    case m.state of
      _ | holds -> pure ()
      Engaged is -> setMonsterState mid (if length is > 1 then Engaged (filter (/= iid) is) else Ready)
      _ -> pure ()
    b <- monsterBehavior mid
    own <- b.afterDisengage mid iid
    pushAll (own <> [CheckReactions (AfterDisengage iid mid) []])
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
      _ -> do
        {- An "Encounter:" ability replaces the encounter, so it is offered beside
        it -- but not where an anomaly has already replaced what would be read. -}
        abilities <- if anomaly then pure [] else encounterAbilitiesFor iid
        let normal = [ResolveEncounterFrom iid (if anomaly then AnomalyDeck else deck), EndEncounterTurn iid]
        if null abilities
          then pushAll normal
          else
            chooseFor iid "Resolve an encounter, or an encounter ability"
              $ label "Resolve an encounter" normal
              : [ Choice (TextLabel a.label) [UseEncounterAbility iid ref n, EndEncounterTurn iid]
                | (ref, n, a) <- abilities
                ]
  UseEncounterAbility iid ref n ->
    lookupEncounterAbility iid ref n >>= traverse_ \a -> a.perform (EffectCtx iid (refSource ref) Nothing)
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
    menc <- use #encounter
    finishEncounter
    for_ menc \enc ->
      pushAll
        $ [CheckReactions (AfterStreetEncounter enc.investigator) [] | enc.deck == StreetDeck]
        <> [CheckReactions (AfterEncounter enc.investigator) []]
  EndEncounterTurn iid -> do
    investigatorL iid . #active .= True
    #turn .= Nothing
    push NextEncounterTurn
  -- Mythos phase (rule 204)
  BeginMythosPhase -> do
    #sheetTokens %= Map.filterWithKey (\k _ -> not ("reckoning-held:" `T.isPrefixOf` k))
    enterPhase MythosPhase
    order <- playerOrder
    push (MythosTurn order)
  MythosTurn [] -> push EndRound
  MythosTurn (p : ps) -> pushAll [DrawMythosToken p, DrawMythosToken p, MythosTurn ps]
  DrawMythosToken pid -> do
    offers <- investigatorOfPlayer pid >>= maybe (pure []) mythosDrawOffers
    if null offers
      then push (DrawMythosTokenNow pid)
      else
        ask pid "Draw a mythos token?"
          $ Choice (DoneLabel "Draw the token") [DrawMythosTokenNow pid]
          : [Choice (TextLabel r.label) r.messages | r <- offers]
  DrawMythosTokenNow pid -> do
    cup <- use #cup
    when (null cup) do
      drawn <- use #drawnTokens
      #drawnTokens .= []
      returnTokensToCup drawn
    cup' <- use #cup
    mi <- if null cup' then pure Nothing else Just <$> randomR (0, length cup' - 1)
    for_ mi \idx -> do
      let tok = cup' !! idx
      #cup .= take idx cup' <> drop (idx + 1) cup'
      #drawnTokens %= (<> [tok])
      #activeToken ?= tok
      logText ("Mythos: " <> tshow tok)
      -- the token is read, then put away; its effect resolves without it on show
      pushAll
        [AcknowledgeMythosToken pid tok, ClearActiveToken, ResolveMythosToken pid tok, CheckStateTriggers]
  {- A card its drawer holds may answer the token flatly (TAINTED's doom); it
  lands behind whatever the token itself sets going. -}
  ResolveMythosToken pid tok -> do
    answers <- investigatorOfPlayer pid >>= maybe (pure []) (`afterMythosTokenFor` tok)
    pushAll (ResolveMythosTokenNow pid tok : answers)
  ResolveMythosTokenNow pid tok -> do
    mine <- investigatorOfPlayer pid
    -- queued first so the token's own resolution, pushed below, still lands ahead of it
    maybe (pure []) (`codexTokenDrawn` tok) mine >>= pushAll
    case tok of
      SpreadDoomToken -> push SpreadDoom
      SpawnMonsterToken -> push (SpawnMonsterAt Nothing False)
      ReadHeadlineToken -> investigatorOfPlayer pid >>= traverse_ (push . DrawHeadline)
      SpawnClueToken -> push SpawnClue
      GateBurstToken -> push GateBurst
      ReckoningToken -> reckoningSources >>= push . ResolveReckonings
      BlankToken -> investigatorOfPlayer pid >>= traverse_ (\iid -> push (CheckReactions (DrewBlankToken iid) []))
      {- A white marker is taken out of the cup for good: whichever card put it there
      says where it goes, so it is never among the tokens returned when the cup runs
      out. -}
      WhiteMarkerToken ->
        #drawnTokens %= \ts -> case break (== WhiteMarkerToken) ts of
          (before, _ : after) -> before <> after
          _ -> ts
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
    investigatorL ms.investigator . #spacesMoved += 1
    investigatorL ms.investigator . #spacesMovedThisRound += 1
    from <- fromJustNote "space" <$> investigatorSpace ms.investigator
    -- a vehicle takes whoever is standing there along (Delivery Truck)
    driving <- hasAssetWith ms.investigator (.carriesPassengers)
    riders <-
      if driving
        then do
          here <- investigatorsAt from
          free <- filterM (fmap null . engagedMonsters . (.id)) here
          pure [i.id | i <- free, i.id /= ms.investigator]
        else pure []
    unless (null riders) $ pushEnd (OfferRide riders sid)
    board <- use #board
    investigatorL ms.investigator . #space ?= sid
    moveEngagedWatchers ms.investigator sid
    engaged <- enterWith ms sid
    unless engaged $ case borderHazard from sid board of
      Just hz | canContinue ms -> hazardPrompt ms hz
      _ -> push (MoveStep ms)
  UseTravelRoute ms sid -> do
    spendMoney ms.investigator 1
    investigatorL ms.investigator . #space ?= sid
    moveEngagedWatchers ms.investigator sid
    engaged <- enterWith ms sid
    unless engaged $ push (MoveStep ms)
  OfferRide [] _ -> pure ()
  OfferRide (iid : rest) sid -> do
    playing <- investigatorIsPlaying iid
    if not playing
      then push (OfferRide rest sid)
      else do
        name <- (.name) <$> getSpace sid
        chooseFor
          iid
          ("Ride along to " <> name <> "?")
          [ Choice (SpaceLabel sid) [MoveDirectly iid sid, OfferRide rest sid]
          , Choice (DoneLabel "Stay") [OfferRide rest sid]
          ]
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
      $ push (PreventHarm (HarmPlan iid src kind dmg hor Nothing Nothing) [])
  {- Prevention comes before the harm is assigned, and the card doing it may belong to
  anyone, so each holder is asked in turn. What a card prevents is reported through
  damagePrevented and horrorPrevented rather than returned, because a prevention may
  cast a spell of its own, which asks questions and may even defeat its caster; this
  step then runs again behind whatever that set going. -}
  PreventHarm plan0 declined -> do
    stoppedDamage <- use #damagePrevented
    stoppedHorror <- use #horrorPrevented
    #damagePrevented .= 0
    #horrorPrevented .= 0
    let plan =
          plan0
            & #damage
            .~ max 0 (plan0.damage - stoppedDamage)
            & #horror
            .~ max 0 (plan0.horror - stoppedHorror)
    when (stoppedDamage > 0) $ logText ("Prevented " <> tshow stoppedDamage <> " damage")
    when (stoppedHorror > 0) $ logText ("Prevented " <> tshow stoppedHorror <> " horror")
    tested <- damagePreventionsFor plan
    simple <- harmPreventers plan.investigator
    let again k = PreventHarm plan (k : declined)
        oneHarm (cid, cardName) =
          [ Reaction
              (cardName <> "-damage")
              (cardName <> ": prevent one damage")
              [MarkAssetUsed plan.investigator cid, PreventedHarm 1 0]
          | plan.damage > 0
          ]
            <> [ Reaction
                   (cardName <> "-horror")
                   (cardName <> ": prevent one horror")
                   [MarkAssetUsed plan.investigator cid, PreventedHarm 0 1]
               | plan.horror > 0
               ]
        offers = tested <> [(plan.investigator, r) | c <- simple, r <- oneHarm c]
    case [(owner, r) | (owner, r) <- offers, r.key `notElem` declined] of
      [] -> push (HarmDamageStage plan)
      ((owner, r) : _) -> do
        let name = maybe "an investigator" (.name) (investigatorDef plan.investigator)
        chooseFor owner ("Prevent harm to " <> name <> "?")
          $ [ Choice (DoneLabel "Skip") [again r.key]
            , Choice (TextLabel r.label) (r.messages <> [again r.key])
            ]
  PreventedHarm d h -> do
    #damagePrevented += d
    #horrorPrevented += h
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
    when (spent > 0) $ push (CheckReactions (AfterSpendRemnant iid) [])
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
  DefeatInvestigator iid -> do
    removeInvestigator iid Defeated True
    -- a card may want them kept rather than put away (Scraping at the Door)
    codexInvestigatorDefeated iid >>= pushAll
  DevourInvestigator iid -> removeInvestigator iid Devoured True
  RetireInvestigator iid -> removeInvestigator iid Retired False
  RecoverInvestigator iid hp sp -> do
    investigatorL iid . #damage %= max 0 . subtract hp
    investigatorL iid . #horror %= max 0 . subtract sp
    when (sp > 0) $ offerRecoverySanity (RecoveredInvestigator iid) =<< investigatorSpace iid
  RecoverAsset cid hp sp -> do
    assetL cid . #damage %= max 0 . subtract hp
    assetL cid . #horror %= max 0 . subtract sp
    when (sp > 0) do
      owner <- uses #assets (fmap (.owner) . Map.lookup cid)
      here <- maybe (pure Nothing) investigatorSpace owner
      offerRecoverySanity (RecoveredAsset cid) here
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
        {- Pursuit: struck from somewhere it cannot reach back to, the monster
        comes after whoever did it. A blow that finishes it earns no chase. -}
        chase <- case src of
          SourceInvestigator iid | Pursuit `elem` d.keywords -> do
            there <- investigatorSpace iid
            pure
              [ MonsterStep mid d.speed (TowardPrey (NamedInvestigator iid))
              | there `notElem` [Nothing, Just m'.space]
              ]
          _ -> pure []
        let defeated = maybe False (m'.damage >=) mh
        pushAll $ (if defeated then [DefeatMonster mid src] else chase) <> answers
  DefeatMonster mid src -> do
    logText "Monster defeated"
    answers <- codexAboutDefeat mid src
    -- read the monster's traits while it is still on the board
    fromCards <- cardsAboutDefeat mid src
    own <- monsterBehavior mid >>= \b -> b.afterDefeated mid src
    beside <- case src of
      SourceInvestigator iid -> do
        ms <- filter ((/= mid) . (.card)) <$> engagedMonsters iid
        concat <$> for ms \m -> monsterBehavior m.card >>= \b -> b.afterAnotherDefeated m.card mid iid
      _ -> pure []
    pushAll (DiscardMonster mid : answers <> fromCards <> own <> beside)
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
    d <- monsterDef mid
    stops <- if d.epic then pure [] else placementStops (PlacingMonster mid) sid
    case stops of
      [] -> push (PlaceMonsterNow mid sid state)
      ((owner, r) : _) ->
        chooseFor
          owner
          "Let it come?"
          [ Choice (DoneLabel "Let it come") [PlaceMonsterNow mid sid state]
          , Choice (TextLabel r.label) r.messages
          ]
  PlaceMonsterNow mid sid state -> do
    #monsters
      . at mid
      ?= Monster {card = mid, space = sid, state, damage = 0, markers = [], prey = Nothing}
    answers <- codexAboutMonster (.afterMonsterSpawn) mid
    everyone <- playingInvestigators
    pushAll
      $ (MonsterEngagesIn mid sid : answers)
      <> [CheckReactions (AfterMonsterSpawned i.id mid) [] | i <- everyone]
  AttackDamage iid mid n -> do
    exists <- uses #monsters (Map.member mid)
    when exists do
      before <- (.damage) <$> getMonster mid
      pushAll [DealMonsterDamage mid (SourceInvestigator iid) n, AttackResolved iid mid before]
  ChooseAttackTarget iid -> do
    msid <- investigatorSpace iid
    ms <- maybe (pure []) monstersAt msid
    chooseFor
      iid
      "Choose a monster to attack"
      [Choice (MonsterLabel m.card) [AttackMonster iid m.card] | m <- ms]
  AttackMonster iid mid -> do
    m <- getMonster mid
    d <- monsterDef mid
    unless (m.state == Exhausted) $ engage iid mid
    -- attacking provokes it even if the attack cannot engage it
    #provoked %= Map.insertWith (<>) mid [iid]
    attackMod <- readMonsterModifier iid mid AttackModifier d.attackModifier
    let attackTest skill = newTest iid skill attackMod (ActionTest AttackAction (Just mid)) (AfterAttack iid mid)
        attackWith skill = BeginTest (attackTest skill)
    -- a card like Storm of Spirits offers another skill in place of the monster's;
    -- its attack modifier applies either way
    alternatives <- attackSkillAlternatives iid d.attackSkill
    if null alternatives
      then push (attackWith d.attackSkill)
      else do
        offers <- for alternatives \(c, skill) -> testWithCard iid c (attackTest skill)
        chooseFor iid "Choose the skill to test"
          $ label ("Test " <> T.toLower (tshow d.attackSkill)) [attackWith d.attackSkill]
          : offers
  AttackResolved iid mid before -> do
    mm <- use (#monsters . at mid)
    dealt <- case mm of
      Nothing -> do
        d <- monsterDef mid
        when d.remnant $ push (GainRemnants iid 1)
        pure True
      Just m -> pure (m.damage > before)
    gone <- uses #monsters (not . Map.member mid)
    retaliators <- filterM (hasKeyword Retaliate . (.card)) =<< engagedMonsters iid
    b <- monsterBehavior mid
    own <- if gone then pure [] else b.afterAttackAction mid iid dealt
    -- "even if you defeat it", so this one is asked of a monster already gone
    exacted <- if dealt then b.afterDamagedInAttack mid iid else pure []
    pushAll
      $ own
      <> exacted
      <> [MonsterAttacks r.card iid | r <- retaliators, r.card /= mid || not dealt]
      <> [CheckReactions (AfterDamageMonsterInAttack iid mid) [] | dealt]
      <> [CheckReactions (AfterDefeatMonsterInAttack iid) [] | gone]
  ClearSpaceDoom sid -> spaceL sid . #doom .= 0
  WardRemove iid sid n -> do
    meddles <- hasAssetWith iid (.wardAlternative)
    if meddles
      then push (WardStep iid sid n 0)
      else do
        s <- getSpace sid
        let k = min n s.doom
        when (k >= 2) $ push (GainRemnants iid 1)
        pushAll [RemoveDoom sid k, CheckReactions (AfterDoomRemoved iid k) []]
  WardStep iid sid left removed -> do
    s <- getSpace sid
    ms <- monstersAt sid
    exhaustable <- filterM (canBeExhausted . (.card)) ms
    names <- for exhaustable \m -> (m.card,) . (.name) <$> getCardDef m.card
    let finish =
          [GainRemnants iid 1 | removed >= 2] <> [CheckReactions (AfterDoomRemoved iid removed) []]
        options =
          [ label "Remove one doom" [RemoveDoom sid 1, WardStep iid sid (left - 1) (removed + 1)]
          | s.doom > 0
          ]
            <> [ Choice
                   (CardsLabel ("Exhaust " <> nm) [mid])
                   [ExhaustMonster mid, WardStep iid sid (left - 1) removed]
               | (mid, nm) <- names
               ]
    if left <= 0 || null options
      then pushAll finish
      else
        chooseFor iid ("Spend a success (" <> tshow left <> " left)")
          $ options
          <> [Choice (DoneLabel "Stop") [WardStep iid sid 0 removed]]
  PayMoney iid n -> spendMoney iid n
  BuyFromDisplayMore ctx mtrait pricing limit ifBought n -> buyPrompt ctx mtrait pricing limit ifBought n
  EvadeMonsters iid n -> do
    ms <- filterM (fmap not . monsterHoldsItsQuarry) . map (.card) =<< engagedMonsters iid
    if n >= length ms
      then do
        for_ ms \mid -> do
          own <- monsterBehavior mid >>= \b -> b.afterEvaded mid iid
          pushAll
            $ [DisengageMonster iid mid, ExhaustMonster mid]
            <> own
            <> [CheckReactions (AfterEvadeMonster iid mid) []]
        unless (null ms) $ investigatorL iid . #bonusActions += 1
      else
        when (n > 0)
          $ chooseFor
            iid
            "Choose a monster to evade"
            [ Choice (MonsterLabel m) [EvadedMonster iid m, EvadeMonsters iid (n - 1)]
            | m <- ms
            ]
  MonstersWatchAction iid kind -> do
    ms <- engagedMonsters iid
    answers <- for ms \m -> monsterBehavior m.card >>= \b -> b.afterAction m.card iid kind
    pushAll (concat answers)
  MonsterEngaged iid mid -> do
    here <- uses #monsters (Map.member mid)
    when here $ monsterBehavior mid >>= \b -> b.afterEngaged mid iid >>= pushAll
  EvadedMonster iid mid -> do
    own <- monsterBehavior mid >>= \b -> b.afterEvaded mid iid
    pushAll
      $ [DisengageMonster iid mid, ExhaustMonster mid]
      <> own
      <> [CheckReactions (AfterEvadeMonster iid mid) []]
  -- Doom and clues (rules 406, 412, 423, 461)
  {- A card may stop doom being put down in its owner's neighborhood, so the ones
  that could are asked before it lands. -}
  {- A piece of map arriving part way through a scenario. It is built on its own
  with the tile it hangs off at the origin, so everything it carries is shifted
  onto that tile's live position and merged in; the union keeps the board's own
  entry for anything it already has, so the tile it is laid against is untouched. -}
  AddToBoard against piece -> do
    board <- use #board
    let added = buildBoard piece
        placedAt ts = listToMaybe [t | t <- ts, t.neighborhood == against]
    case (placedAt board.layout.tiles, placedAt piece.layout.tiles) of
      (Just live, Just origin) -> do
        let dx = live.x - origin.x
            dy = live.y - origin.y
            known = Map.keysSet board.neighborhoods
            here = Map.keysSet board.spaces
            fresh = Map.keysSet added.neighborhoods `Set.difference` known
        #board . #neighborhoods %= (<> added.neighborhoods)
        #board . #spaces %= (<> added.spaces)
        #board . #borders %= \old -> Map.unionWith (<>) old added.borders
        #board
          . #layout
          . #tiles
          %= ( <>
                 [ TilePlacement t.neighborhood (t.x + dx) (t.y + dy)
                 | t <- added.layout.tiles
                 , t.neighborhood `Set.member` fresh
                 ]
             )
        #board
          . #layout
          . #streets
          %= ( <>
                 [ StreetPlacement p.space (p.x + dx) (p.y + dy) p.angle
                 | p <- added.layout.streets
                 , not (p.space `Set.member` here)
                 ]
             )
        #board
          . #layout
          . #anchors
          %= ( <>
                 [ SpaceAnchor a.space (a.x + dx) (a.y + dy)
                 | a <- added.layout.anchors
                 , not (a.space `Set.member` here)
                 ]
             )
        {- A threshold piece laid down now has its icons printed on it the same way one
        laid at setup does, so it is turned the same way too; otherwise the board shows
        one hazard while the engine charges another. -}
        let laid =
              [ s.id
              | s <- Map.elems added.spaces
              , not (s.id `Set.member` here)
              , case s.kind of ThresholdSpace _ -> True; _ -> False
              ]
        traverse_ turnThresholdTile laid
        for_ (Map.elems (Map.restrictKeys added.neighborhoods fresh)) \n ->
          logText (n.name <> " is added to the board")
      _ -> logText "Nothing on the board to add that map against"
  PlaceDoom src sid -> whenSpaceExists sid do
    stops <- placementStops PlacingDoom sid
    case stops of
      [] -> push (PlaceDoomNow src sid)
      ((owner, r) : _) ->
        chooseFor
          owner
          "Let the doom fall?"
          [ Choice (DoneLabel "Let it fall") [PlaceDoomNow src sid]
          , Choice (TextLabel r.label) r.messages
          ]
  -- a space a scenario has taken off the board simply takes no more doom
  PlaceDoomNow src sid -> whenSpaceExists sid do
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
  PlaceDoomOnSheet n ->
    sheetDoomInstead n >>= \case
      Just instead -> pushAll instead
      Nothing -> do
        #sheetDoom += n
        everyone <- playingInvestigators
        pushAll
          $ [CheckReactions (AfterDoomOnSheet i.id n) [] | n > 0, i <- everyone]
          <> [CheckStateTriggers]
  RemoveDoom sid n -> do
    s <- getSpace sid
    let k = min n s.doom
    spaceL sid . #doom -= k
    for_ s.neighborhood \nid -> do
      total <- uses #board (neighborhoodDoom nid)
      when (total == 0) $ neighborhoodL nid . #anomaly .= False
    push CheckStateTriggers
  PlaceMonsterMarker mid colour -> do
    exists <- uses #monsters (Map.member mid)
    when exists do
      monsterL mid . #markers %= (<> [Marker colour True])
      logText ("A " <> colour <> " marker is placed on a monster")
  PlaceMarkerFacedown sid colour -> do
    s <- getSpace sid
    spaceL sid . #markers %= (<> [Marker colour False])
    logText ("A marker is placed face down at " <> s.name)
  RevealMarkerAt sid -> do
    s <- getSpace sid
    case [m | m <- s.markers, not m.faceUp] of
      [] -> logText ("Nothing is hidden at " <> s.name)
      (m : _) -> do
        spaceL sid . #markers %= turnOne
        logText ("The marker at " <> s.name <> " is " <> m.color)
        push CheckStateTriggers
   where
    turnOne ms = case break (not . (.faceUp)) ms of
      (before, m : after) -> before <> (m {faceUp = True} : after)
      _ -> ms
  PlaceBystander sid ->
    use (#decks . #ally) >>= \case
      [] -> logText "No ally card is left to stand in for a bystander"
      (cid : rest) -> do
        #decks . #ally .= rest
        #bystanders %= Just . (<> [(cid, sid)]) . fromMaybe []
        s <- getSpace sid
        logText ("A bystander is left at " <> s.name)
  -- the card was face down, so what they have saved is only now known
  TakeBystander iid cid -> do
    standing <- uses #bystanders (any ((== cid) . fst) . fromMaybe [])
    when standing do
      #bystanders %= fmap (filter ((/= cid) . fst))
      d <- getCardDef cid
      logText (d.name <> " is helped to safety")
      pushAll [GainAsset iid cid, CheckStateTriggers]
  DiscardBystander cid -> do
    standing <- uses #bystanders (any ((== cid) . fst) . fromMaybe [])
    when standing do
      #bystanders %= fmap (filter ((/= cid) . fst))
      #decks . #ally %= (<> [cid])
      d <- getCardDef cid
      logText (d.name <> " is lost to the monsters")
      push CheckStateTriggers
  MoveCornerTile piece around -> moveCornerTile piece around
  TakeClues iid n -> addClues iid n
  MarkCodexToken card name k -> do
    #codex
      . traversed
      . filtered ((== card) . (.number))
      . #tokens
      . at name
      %= Just
      . max 0
      . (+ k)
      . fromMaybe 0
    push CheckStateTriggers
  PlaceNeighborhoodMarker nid colour faceUp -> do
    n <- getNeighborhood nid
    neighborhoodL nid . #markers %= (<> [Marker colour faceUp])
    logText ("A " <> colour <> " marker is placed in " <> n.name)
  AddSheetClues n -> do
    #sheetClues += n
    push CheckStateTriggers
  DiscardMarkers colour -> do
    let drop' = filter ((/= colour) . (.color))
    #board . #spaces . traversed . #markers %= drop'
    #board . #neighborhoods . traversed . #markers %= drop'
    logText ("All " <> colour <> " markers are discarded")
  PlaceMarker sid colour -> do
    s <- getSpace sid
    spaceL sid . #markers %= (<> [Marker colour True])
    logText ("A " <> colour <> " marker is placed at " <> s.name)
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
  SpawnClue -> spawnOneClue False
  SpawnClueOnTop -> spawnOneClue True
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
  {- A card that bans conditions or traits discards what its new owner already
  holds; a card whose own trait is banned never arrives at all. -}
  GainNow ctx g -> gain ctx g
  GainAsset iid cid -> do
    applyBans iid cid
    refused <- traitIsBanned iid cid
    if refused then push (DiscardAsset cid) else gainAsset iid cid
  DiscardAsset cid -> discardAsset cid
  GainNamedCard iid name -> do
    pile <- use (#decks . #special)
    matches <- filterM (cardMatches (NamedCard name)) pile
    case matches of
      (cid : _) -> pushAll [GainAsset iid cid, AfterGainedFromDeck iid cid]
      [] -> logText ("Special card unavailable: " <> name)
  GainConditionMsg iid name -> gainCondition False iid name
  GainAnotherCondition iid name -> gainCondition True iid name
  FocusSkill iid skill evenIfExceeds -> do
    i <- getInvestigator iid
    most <- focusPerSkillFor iid
    let on = Map.findWithDefault 0 skill i.focus
    when (on < most) do
      investigatorL iid . #focus . at skill ?= on + 1
      checkFocusLimit iid evenIfExceeds
  {- A second token on a skill already focused, for a card that says so outright
  (Life of Privilege, Just That Good). The per-skill limit is what the card lifts;
  the focus limit still holds. -}
  FocusSkillAgain iid skill -> do
    investigatorL iid . #focus . at skill %= Just . maybe 1 (+ 1)
    checkFocusLimit iid False
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
  MarkSheetToken name n -> do
    #sheetTokens . at name %= Just . max 0 . (+ n) . fromMaybe 0
    push CheckStateTriggers
  MarkSheet n -> do
    #sheetMarkers += n
    push CheckStateTriggers
  DiscardClue Nothing -> #sheetClues %= max 0 . subtract 1
  DiscardClue (Just iid) -> addClues iid (-1)
  ResearchClues iid n -> do
    i <- getInvestigator iid
    let maxN = min n i.clues
    -- the result is worth answering even when there were no clues to move
    pushEnd (CheckReactions (AfterResearchResult iid n) [])
    when (maxN > 0)
      $ chooseFor
        iid
        "Research clues"
        [Choice (AmountLabel k) [ResearchCluesExact iid k] | k <- [0 .. maxN]]
  ResearchCluesExact iid k -> do
    addClues iid (negate k)
    instead <- sheetCluesInstead k
    pushAll
      (CheckReactions (AfterCluesResearched iid k) [] : fromMaybe [AddSheetClues k] instead)
  TradeWith iid other -> tradePrompt iid other
  TradeTransfer giver receiver item -> do
    case item of
      TradeMoney n -> addMoney giver (negate n) >> addMoney receiver n
      TradeClues n -> addClues giver (negate n) >> addClues receiver n
      TradeRemnants n -> addRemnants giver (negate n) >> addRemnants receiver n
      TradeFocus skill -> do
        investigatorL giver . #focus . at skill %= \case
          Just n | n > 1 -> Just (n - 1)
          _ -> Nothing
        investigatorL receiver . #focus . at skill %= Just . maybe 1 (+ 1)
      TradeCard cid -> do
        used <- elem cid . (.usedAssets) <$> getInvestigator giver
        investigatorL giver . #assets %= filter (/= cid)
        investigatorL receiver . #assets %= (<> [cid])
        assetL cid . #owner .= receiver
        when used $ investigatorL receiver . #lockedAssets %= (<> [cid])
  {- A card may swap something into the display before its owner shops (Eye for
  Appraisal), so the prices are read only once the shelf is settled. -}
  BuyFromDisplayMsg ctx mtrait pricing limit ifBought -> do
    offers <- reactionsFor (BeforeAcquiring ctx.investigator mtrait)
    pushAll
      $ [CheckReactions (BeforeAcquiring ctx.investigator mtrait) [] | not (null offers)]
      <> [BuyFromDisplayNow ctx mtrait pricing limit ifBought]
  BuyFromDisplayNow ctx mtrait pricing limit ifBought -> do
    let iid = ctx.investigator
        buy = BuyFromDisplayChecked ctx mtrait pricing limit ifBought
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
  BuyFromDisplayChecked ctx mtrait pricing limit ifBought -> buyPrompt ctx mtrait pricing limit ifBought 0
  BuyCard iid cid price -> do
    spendMoney iid price
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
  RevealArchiveCard n after -> do
    found <- archiveCardFor n
    case found of
      Just cid -> do
        #activeCard ?= cid
        d <- getCardDef cid
        logText ("Turned up: " <> (if d.name == "" then "card " <> tshow (coerce n :: Int) else d.name))
        askLeader
          ("Card " <> tshow (coerce n :: Int) <> " turned up")
          [Choice (DoneLabel "Continue") (ClearActiveCard cid : after)]
      Nothing -> pushAll after
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
  MarkAbilityUsed iid key -> investigatorL iid . #usedAbilities %= (<> [key])
  {- A card that turns itself over the moment it arrives shows a side nobody has
  read: the instructions that put it there. The table says when it may turn. -}
  TurnCodexCard n ->
    askLeader
      ("Card " <> tshow (coerce n :: Int) <> " read")
      [Choice (DoneLabel "Continue") [FlipCodexCard n]]
  FlipCodexCard n -> do
    #codex %= map (\e -> if e.number == n then e {flipped = not e.flipped, fired = []} else e)
    codexChanged
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
        -- a codex card may have shuffled something in that is no headline at all
        instead <- codexHeadlineInstead iid cid
        case instead of
          Just msgs -> pushAll (msgs <> [ClearActiveCard cid])
          Nothing -> case d.kind of
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
  ResolveReckonings ss -> do
    -- a card may ask to go last, which the leader's free choice of order would break
    late <- filterM isLateReckoning ss
    let offer = case filter (`notElem` late) ss of [] -> ss; early -> early
    askLeader
      "Choose the next reckoning to resolve"
      [Choice (SourceLabel s) [ResolveReckoning s, ResolveReckonings (filter (/= s) ss)] | s <- offer]
  ResolveReckoning src -> do
    held <- uses #sheetTokens (Map.member (reckoningHeldKey src))
    offers <- if held then pure [] else reckoningOffers src
    if
      | held -> logText "That reckoning does not resolve this mythos phase"
      | null offers -> push (ResolveReckoningNow src)
      | otherwise ->
          askLeader "Answer this reckoning?"
            $ Choice (DoneLabel "Let it resolve") [ResolveReckoningNow src]
            : [Choice (TextLabel r.label) r.messages | r <- offers]
  ResolveReckoningNow src -> resolveReckoning src
  CancelReckoning key -> #sheetTokens . at key ?= 1
  -- Effects and tests
  ResolveEffect ctx eff -> resolveEffect ctx eff
  PayCost ctx cost -> payCost ctx cost
  BeginTest ts -> beginTest ts
  RememberOnCard cid what -> assetL cid . #tokens .= Map.singleton what 1
  NoteOnCard cid what n -> assetL cid . #tokens . at what ?= n
  {- 429.9: a remnant is gained one at a time, and a card may take something else in
  its place, so each one is offered before it lands. -}
  GainRemnants iid n
    | n <= 0 -> pure ()
    | otherwise -> do
        offers <- remnantReplacementsFor iid
        if null offers
          then addRemnants iid n
          else
            chooseFor
              iid
              "Gain a remnant?"
              ( Choice (DoneLabel "Gain the remnant") [GainRemnantsNow iid 1, GainRemnants iid (n - 1)]
                  : [Choice (TextLabel r.label) (r.messages <> [GainRemnants iid (n - 1)]) | r <- offers]
              )
  GainRemnantsNow iid n -> do
    addRemnants iid n
    when (n > 0) $ afterGainRemnantFor iid >>= pushAll
  RaiseInsteadOfReroll cost idx cid -> do
    ts <- fromJustNote "no test" <$> use #test
    payRerollCost ts.investigator cost
    investigatorL ts.investigator . #usedAssets %= (<> [cid])
    #test . _Just . #dice . ix idx . #value += 1
    testPrompt
  ToggleTestAsset cid -> toggleTestAsset cid
  SetTestSkill skill -> do
    #test . _Just . #skill .= skill
    push ContinueTest
  AttachAsset cid target -> do
    assetL cid . #attachedTo ?= target
    name <- (.name) <$> getCardDef cid
    onto <- (.name) <$> getCardDef target
    logText (name <> " is attached to " <> onto)
  RollDice -> rollTestDice
  SpendForReroll cost -> chooseRerollDie cost
  RerollDie cost idx -> rerollDie cost idx
  RerollUpTo src n -> rerollUpToPaying src n
  RerollUpToNow src n -> rerollUpTo src n
  RerollOneOf src n idx -> rerollOneOf src n idx
  RerollAll src -> rerollAll src
  RollAdditionalDice src n -> rollAdditionalDice src n
  RollADiePerFailure src -> rollADiePerFailure src
  RemoveADie src -> removeADie src
  RemoveDieAt idx -> #test . _Just . #dice . ix idx . #removed .= True
  AddToDie src -> chooseDieToRaise src
  ChooseDieToSet n -> chooseDieToSet n
  {- Lucky Coin names the result before the die, since the die it changes is the
  one the result is wanted on. -}
  ChooseDieResult ->
    use #test >>= traverse_ \ts ->
      chooseFor
        ts.investigator
        "Choose the die's new result"
        [Choice (AmountLabel v) [ChooseDieToSet v] | v <- [1 .. 6]]
  SetDieValue idx n -> setDieValue idx n
  AddTestRider ctx eff -> do
    inTest <- uses #test isJust
    if inTest
      then #test . _Just . #riders %= (<> [(ctx, eff)])
      else #pendingRiders %= Just . (<> [(ctx, eff)]) . fromMaybe []
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

{- | Every investigator from every box, whichever expansions are on the table: the
chosen expansions pick the scenario and its content, not who may play it. A
possession from a box that is not in play is minted rather than taken
('GainNamedStarting').
-}
availableInvestigators :: GameM [InvestigatorDef]
availableInvestigators = do
  used <- use #usedInvestigators
  let usedNames = [d.name | iid <- used, Just d <- [investigatorDef iid]]
  pure
    [ d
    | d <- Map.elems investigatorDefs
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
  {- A card that matches somebody else's pool reads it off the action just taken, so
  the count starts again here and stays at nothing for an action with no test. -}
  investigatorL iid . #lastTestDice .= Nothing
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
      -- a card may keep monsters off them for the length of the action (Chuck Fergus)
      unseen <- hasAssetWith iid (.ignoredWhileMoving)
      let normal = [MoveStep (MoveState iid 2 0 2 True unseen), after]
      if null vehicles && null spells
        then pushAll normal
        else
          chooseFor iid "Move"
            $ label "Move normally" normal
            : [ Choice (CardLabel c) [MarkAssetUsed iid c, MoveStep (MoveState iid steps 0 paid True unseen), after]
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
        -- the skill that was focused is read off the choice, for a card that adds
        -- to that same one (Life of Privilege)
        [ Choice (SkillLabel s) [FocusSkill iid s False, CheckReactions (AfterFocusedSkill iid s) [], after]
        | s <- options
        ]
    WardAction -> do
      alternatives <- codexWardSkills
      let attempt s = BeginTest (newTest iid s 0 (ActionTest WardAction Nothing) (AfterWard iid sid))
      case alternatives of
        [] -> pushAll [attempt Lore, after]
        _ ->
          chooseFor
            iid
            "Choose the skill for the ward"
            [Choice (SkillLabel s) [attempt s, after] | s <- Lore : alternatives]
    ResearchAction ->
      pushAll
        [ BeginTest (newTest iid Observation 0 (ActionTest ResearchAction Nothing) (AfterResearch iid))
        , after
        ]
    EvadeAction -> do
      ms <- engagedMonsters iid
      mods <- for ms \m ->
        readMonsterModifier iid m.card EvadeModifier . (.evadeModifier) =<< monsterDef m.card
      i <- getInvestigator iid
      -- Mists of R'lyeh offers lore in place of observation; the monster's evade
      -- modifier applies either way
      alternatives <- fmap catMaybes $ for [c | c <- i.assets, c `notElem` i.lockedAssets] \c ->
        fmap (c,) . (.evadeSkillInstead) <$> assetBehavior c
      let evadeTest skill = newTest iid skill (minimum mods) (ActionTest EvadeAction Nothing) (AfterEvade iid)
      if null alternatives
        then pushAll [BeginTest (evadeTest Observation), after]
        else do
          offers <- for alternatives \(c, skill) -> testWithCard iid c (evadeTest skill)
          chooseFor iid "Choose the skill to test"
            $ label "Test observation" [BeginTest (evadeTest Observation), after]
            : [Choice l (ms' <> [after]) | Choice l ms' <- offers]
    {- The target is chosen after anything printed "before you perform an attack
    action" has run, so a monster hauled in by then can be the one attacked. -}
    AttackAction ->
      pushAll [CheckReactions (BeforePerformAction iid AttackAction) [], ChooseAttackTarget iid, after]
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
          push after
          a.perform (EffectCtx iid (refSource ref) Nothing)

{- | Testing a skill some card of theirs offers in place of the printed one. A
spell is cast to do it, which costs horror and can be interrupted; an item simply
lends the skill.
-}
testWithCard :: InvestigatorId -> CardId -> TestState -> GameM Choice
testWithCard iid cid ts = do
  name <- (.name) <$> getCardDef cid
  isSpell <- maybe False ((== Spell) . (.assetType)) <$> assetDef cid
  let lbl = CardsLabel ("Test " <> T.toLower (tshow ts.skill) <> " with " <> name) [cid]
  pure
    $ Choice lbl
    $ if isSpell
      then [CastSpell iid cid [BeginTest ts {casting = Just cid}]]
      else [BeginTest ts]

-- | The source a component's own ability speaks with.
refSource :: ComponentRef -> Source
refSource = \case
  SheetRef i -> SourceInvestigator i
  CardRef c -> SourceCard c
  CodexRef a -> SourceCodex a

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
noticedEngageOnEntry iid = engageOnEntryWhere notices iid
 where
  -- a monster that holds its quarry passes everyone else by
  notices mid = do
    ignores <- monsterIgnores mid iid
    holds <- monsterHoldsItsQuarry mid
    pure (not ignores && not holds)

moveStep :: MoveState -> GameM ()
moveStep ms = do
  i <- getInvestigator ms.investigator
  board <- use #board
  purse <- availableMoney ms.investigator
  for_ i.space \sid -> do
    adj <- reachable (adjacentSpaces sid board)
    routeSpaces <- reachable (sameRouteSpaces sid board)
    -- a card may throw in a space of its own for the dollar (Cabbie's Favor)
    tips <- extraPaidSteps ms.investigator
    let free = ms.remaining > 0
        paid = not free && ms.paidSteps < ms.maxPaidSteps && purse >= 1
        bought =
          ms
            { paidSteps = ms.paidSteps + 1
            , remaining = ms.remaining + sum (map snd tips)
            }
        marks = [MarkAssetUsed ms.investigator c | (c, _) <- tips]
        stepChoices
          | free = spaceChoices adj \s -> [MoveInvestigator ms {remaining = ms.remaining - 1} s]
          | paid = spaceChoices adj \s -> marks <> [PayMoney ms.investigator 1, MoveInvestigator bought s]
          | otherwise = []
        routes =
          if ms.voluntary && purse >= 1
            then [Choice (SpaceLabel r) [UseTravelRoute ms r] | r <- routeSpaces]
            else []
        choices = stepChoices <> routes
    unless (null choices)
      -- the spaces are picked on the map, so the only button this prompt needs is its last
      $ chooseFor ms.investigator "Move" (choices <> [Choice (DoneLabel "Done moving") []])

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
  refuses <- cannotBeDiscarded cid
  ma <- if refuses then pure Nothing else use (#assets . at cid)
  for_ ma \a -> do
    investigatorL a.owner . #assets %= filter (/= cid)
    #assets . at cid .= Nothing
    b <- assetBehavior cid
    b.onDiscard cid a.owner >>= pushAll
    (investigatorBehavior a.owner).onOwnedDiscard a.owner cid >>= pushAll
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

{- | Hands over a condition by name (415.4, 415.6). @another@ is for a card that says to
take one although its holder has one already ("keep all of them"), which the rules
otherwise refuse.
-}
gainCondition :: Bool -> InvestigatorId -> ConditionName -> GameM ()
gainCondition another iid name = do
  already <- if another then pure False else hasCondition iid name
  banned <- hasAssetWith iid (elem name . (.bansConditions))
  bannedBySheet <- pure (name `elem` (investigatorBehavior iid).bansConditions)
  -- an investigator still joining is being set up, and may start with a condition
  joining <- (== Joining) . (.status) <$> getInvestigator iid
  playing <- (|| joining) <$> investigatorIsPlaying iid
  -- blessed and cursed cancel: "if you would become CURSED, discard this card instead"
  opposing <- case name of
    "BLESSED" -> conditionCard iid "CURSED"
    "CURSED" -> conditionCard iid "BLESSED"
    _ -> pure Nothing
  {- FATIGUED does not cancel with DRIVEN but turns it out: the DRIVEN already held
  goes, and no new one may be taken while the fatigue lasts. -}
  spent <- if name == "FATIGUED" then conditionCard iid "DRIVEN" else pure Nothing
  tooTired <- if name == "DRIVEN" then hasCondition iid "FATIGUED" else pure False
  case opposing of
    _ | banned || bannedBySheet || tooTired -> do
      logText (coerce name <> " cannot be held, and is discarded")
      conditionCard iid name >>= traverse_ (push . DiscardAsset)
    Just cid | playing -> do
      logText "The opposing condition is discarded instead"
      push (DiscardAsset cid)
    _ -> when (playing && not already) do
      for_ spent \cid -> do
        logText "The drive gives out"
        push (DiscardAsset cid)
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
          -- a condition placed this way still sends away what it bans
          applyBans iid cid

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

{- | Puts a card into its new owner's play area. Kept apart from 'GainAsset' so
the bans that card checks do not have to be repeated.
-}
gainAsset :: InvestigatorId -> CardId -> GameM ()
gainAsset iid cid = do
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

-- | What a newly gained card sends away: the conditions and traits it bans.
applyBans :: InvestigatorId -> CardId -> GameM ()
applyBans iid cid = do
  b <- assetBehavior cid
  for_ b.bansConditions \name -> conditionCard iid name >>= traverse_ (push . DiscardAsset)
  for_ b.bansTraits \t -> bannedByTrait iid t >>= traverse_ (push . DiscardAsset)

-- | The cards an investigator holds with that trait, which a ban sends away.
bannedByTrait :: InvestigatorId -> Trait -> GameM [CardId]
bannedByTrait iid t = do
  i <- getInvestigator iid
  filterM (fmap (maybe False (elem t . (.traits))) . assetDef) i.assets

-- | Whether anything the investigator holds, or their own sheet, bans this card.
traitIsBanned :: InvestigatorId -> CardId -> GameM Bool
traitIsBanned iid cid = do
  traits <- maybe [] (.traits) <$> assetDef cid
  fromCards <- hasAssetWith iid (any (`elem` traits) . (.bansTraits))
  pure (fromCards || any (`elem` traits) (investigatorBehavior iid).bansTraits)

{- | How many cards the display holds: five, plus whatever the cards in play and
the rumor in the codex say (429.4).
-}
displaySize :: GameM Int
displaySize = do
  invs <- uses #investigators Map.elems
  fromCards <- fmap sum $ for (concatMap (.assets) invs) \cid -> do
    b <- assetBehavior cid
    b.displayDelta cid
  mrumor <- use #rumor
  shrunk <- case mrumor of
    Just r -> (== "stocks-stutter-as-banks-mutter") <$> cardCode r.card
    Nothing -> pure False
  pure (max 1 (5 + fromCards - (if shrunk then 1 else 0)))

refillDisplay :: GameM ()
refillDisplay = do
  display <- use (#decks . #display)
  deck <- use (#decks . #item)
  want <- displaySize
  let need = want - length display
      (new, rest) = splitAt need deck
  when (need > 0) do
    #decks . #display .= display <> new
    #decks . #item .= rest

buyPrompt :: EffectCtx -> Maybe Trait -> Pricing -> Maybe Int -> Effect -> Int -> GameM ()
buyPrompt ctx mtrait pricing limit ifBought bought = do
  let iid = ctx.investigator
      finish = if bought > 0 then [ResolveEffect ctx ifBought] else [CycleDisplay iid 2]
  markup <- displayMarkup iid
  display <- use (#decks . #display)
  priced <- fmap catMaybes $ for display \cid -> do
    ok <- maybe (pure True) (\t -> cardMatches (WithTrait t) cid) mtrait
    md <- assetDef cid
    pure do
      d <- md
      v <- (+ markup) <$> d.value
      guard ok
      pure (cid, applyPricing pricing v)
  -- a card that halves a price (Fine Clothes, Henry Wan) says it does not stack,
  -- so it is offered only on a purchase that is not halved already
  purse <- availableMoney iid
  discounts <- case pricing of HalfPrice -> pure []; _ -> halfPriceCards iid
  let more = BuyFromDisplayMore ctx mtrait pricing limit ifBought (bought + 1)
      options = [o | o@(_, price) <- priced, price <= purse]
      halved price = (price + 1) `div` 2
  -- what a card offers in place of simply paying (Good Standing's test), one offer
  -- per card on sale, since the price it changes is that card's
  offers <- fmap concat $ for priced \(cid, price) -> do
    rs <- buyOffersFor iid cid price
    pure [Choice (TextLabel r.label) (r.messages <> [more]) | r <- rs]
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
             , halved price <= purse
             ]
          <> offers

-- | What a card on sale costs under the terms of the purchase.
applyPricing :: Pricing -> Int -> Int
applyPricing pricing v = case pricing of
  FullPrice -> v
  HalfPrice -> (v + 1) `div` 2
  FlatPrice flat -> flat
  Markup n -> v + n

{- | Cards answer a recovery of sanity where it happened, so everyone standing
there is asked (Hypnotist's Mirror). A recovery off the board answers to nobody.
-}
offerRecoverySanity :: RecoverTarget -> Maybe SpaceId -> GameM ()
offerRecoverySanity target msid = for_ msid \sid -> do
  here <- investigatorsAt sid
  pushAll [CheckReactions (AfterRecoverSanity i.id target) [] | i <- here]

{- | Where an investigator joining the game stands. A scenario may eat its own
starting space (Tsathoggua devours whole tiles), so what is left of the board
stands in for it.
-}
displayMarkup :: InvestigatorId -> GameM Int
displayMarkup iid = do
  mr <- use #rumor
  ignored <- elem iid <$> use #rumorIgnored
  codes <- traverse (cardCode . (.card)) mr
  pure $ if codes == Just "truckers-strike-leads-to-shortages" && not ignored then 2 else 0

{- | The table's cards hear that the codex has changed (Death). Queued before
whatever the card itself sets going, so it is asked once that has resolved.
-}
codexChanged :: GameM ()
codexChanged = do
  invs <- playingInvestigators
  pushAll [CheckReactions (AfterCodexChanged i.id) [] | i <- invs]

{- | The card in the archive that an archive number names. Most are archive cards
and carry the number themselves; the artifacts printed on one are assets, and only
their code says which number they were.
-}
archiveCardFor :: ArchiveNumber -> GameM (Maybe CardId)
archiveCardFor n = do
  archive <- use (#decks . #archive)
  let numbered d = case d.kind of ArchiveCard a -> a.number == n; _ -> False
      coded d = d.code == CardCode ("archive-" <> tshow (coerce n :: Int))
  listToMaybe <$> filterM (\cid -> getCardDef cid <&> \d -> numbered d || coded d) archive

addToCodex :: ArchiveNumber -> Bool -> GameM ()
addToCodex n flipped = do
  matches <- maybeToList <$> archiveCardFor n
  case matches of
    (cid : _) -> do
      removeCardEverywhere cid
      let entry = CodexEntry {number = n, card = cid, flipped = flipped, tokens = mempty, fired = []}
      #codex %= (<> [entry])
      logText
        ("Card " <> tshow (coerce n :: Int) <> " added to the codex" <> (if flipped then " facedown" else ""))
      codexChanged
      push CheckStateTriggers
      (codexBehavior n).onAdd entry
    [] -> logText ("Archive card unavailable: " <> tshow (coerce n :: Int))

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
    StartingEffect txt _ -> txt
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
  purse <- availableMoney iid
  options <- fmap catMaybes $ for revealed \cid -> do
    mv <- cardValue cid
    pure do
      v <- mv
      let price = applyPricing pricing v
      guard (price <= purse)
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

{- | A card may widen what a trade can carry (Mi-Go Brain Case): talents change
hands as well, and so do focus tokens, so long as the one receiving them stays
within their own limits.
-}
tradePrompt :: InvestigatorId -> InvestigatorId -> GameM ()
tradePrompt iid other = do
  a <- getInvestigator iid
  b <- getInvestigator other
  extended <-
    hasAssetWith iid (.tradesFocusAndTalents) ||^ hasAssetWith other (.tradesFocusAndTalents)
  let isTalent cid = maybe False ((== Talent) . (.assetType)) <$> assetDef cid
      tradable i =
        filterM
          ( \cid ->
              cardMatches ItemCard cid
                ||^ cardMatches AllyCard cid
                ||^ cardMatches SpellCard cid
                ||^ (if extended then isTalent cid else pure False)
          )
          i.assets
  aCards <- tradable a
  bCards <- tradable b
  focusGive <-
    if extended
      then (<>) <$> focusOffers iid other a b <*> focusOffers other iid b a
      else pure []
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
    <> focusGive
 where
  (||^) x y = x >>= \r -> if r then pure True else y
  labelText = \case
    TextLabel t -> t
    other' -> tshow other'
  -- focus the giver holds that the receiver still has room for, per skill and in all
  focusOffers giver receiver g r = do
    perSkill <- focusPerSkillFor receiver
    mlimit <- focusLimit receiver
    let room skill =
          Map.findWithDefault 0 skill r.focus
            < perSkill
            && maybe True (focusCount r <) mlimit
    pure
      [ label
          ((if giver == iid then "Give" else "Take") <> " focus: " <> tshow skill)
          [TradeTransfer giver receiver (TradeFocus skill), TradeWith iid other]
      | (skill, n) <- Map.toList g.focus
      , n > 0
      , room skill
      ]

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
  WhiteMarkerToken -> "White marker"

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

{- | A clue spawning (430.7): the top event card is read, its neighborhood gains
the clue, and the card goes back among the top two of that neighborhood's deck.
A card may say to leave it on top instead, so the clue is where it was put
(Spirit Camera).
-}
spawnOneClue :: Bool -> GameM ()
spawnOneClue onTop = withEventDeck \case
  [] -> pure ()
  (cid : rest) -> do
    #decks . #event .= rest
    #revealedEvent ?= cid
    #activeCard ?= cid
    e <- eventDef cid
    neighborhoodL e.neighborhood . #clues += 1
    nd <- use (#decks . #neighborhoods . at e.neighborhood . non [])
    nd' <- if onTop then pure (cid : nd) else shuffleIntoTopTwo cid nd
    #decks . #neighborhoods . at e.neighborhood ?= nd'
    logText ("A clue spawns in " <> coerce e.neighborhood)
    push (ClearActiveCard cid)

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

{- | Cards that take something else in place of a remnant their owner would gain
(429.9); each is offered as an alternative to the remnant itself.
-}
remnantReplacementsFor :: InvestigatorId -> GameM [Reaction]
remnantReplacementsFor iid = do
  i <- getInvestigator iid
  fmap concat $ for [c | c <- i.assets, c `notElem` i.lockedAssets] \cid -> do
    b <- assetBehavior cid
    b.insteadOfRemnant cid iid

-- | Runs its body only while the space is still on the board.
whenSpaceExists :: SpaceId -> GameM () -> GameM ()
whenSpaceExists sid body = do
  there <- uses (#board . #spaces) (Map.member sid)
  when there body

-- 406.3a, 461.1a, terror (Under Dark Waves)
checkDoomThresholds :: SpaceId -> GameM ()
checkDoomThresholds sid = whenSpaceExists sid do
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
      anomalyOpened nid >>= pushAll
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
        , returnToTop = Nothing
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
        -- a codex card may answer what the encounter says rather than where it was drawn
        override <- encounterOverrideFor iid enc
        unless (T.null enc.text) $ logText enc.text
        pushAll [ResolveEffect ctx (fromMaybe enc.effect override), AcknowledgeEncounter, FinishEncounter]
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
      else
        if enc.returnToTop == Just True
          then deckLens enc.deck %= (enc.card :)
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
  {- Debug puts them down where you drop them: no block is respected and nothing is
  engaged on arrival, so a position can be set up without springing what is there.
  Monsters engaged with them come along, since an engaged monster shares their space. -}
  DebugMoveInvestigator iid sid -> do
    investigatorL iid . #space ?= sid
    moveEngagedWatchers iid sid
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
