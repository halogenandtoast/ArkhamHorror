module AH3e.Engine.Effect (resolveEffect, payCost, evalPredicate) where

import AH3e.Engine.Helpers
import AH3e.Engine.Hooks
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State
import Data.List (nub)
import Data.Map.Strict qualified as Map

resolveEffect :: EffectCtx -> Effect -> GameM ()
resolveEffect ctx eff0 = do
  eff <- fixCounts ctx eff0
  let iid = ctx.investigator
      amt = evalAmount ctx
      again = ResolveEffect ctx
  playing <- investigatorIsPlaying iid
  case eff of
    Seq es -> pushAll (map again es)
    ByResult table -> do
      let r = fromMaybe 0 ctx.testResult
      for_ (listToMaybe [e | ((lo, hi), e) <- table, r >= lo, maybe True (r <=) hi]) (push . again)
    ForEachOf c e -> do
      n <- countOf ctx c
      pushAll (replicate n (again e))
    ForInvestigators scope e -> do
      targets <- scopeInvestigators ctx scope
      pushAll [ResolveEffect (EffectCtx i ctx.source ctx.testResult) e | i <- targets]
    NoEffect -> pure ()
    Test skill modifier onPass onFail
      | playing ->
          push (BeginTest (newTest iid skill modifier EncounterTest (AfterEffect ctx onPass onFail)))
      | otherwise -> push (ResolveEffect ctx {testResult = Just 0} onFail)
    RepeatWhilePaying cost e -> push (again (MayPay cost (Seq [e, RepeatWhilePaying cost e]) NoEffect))
    MayPay cost yes no -> do
      can <- canPayCost iid cost
      if playing && can
        then
          chooseFor
            iid
            "Pay the cost?"
            [label "Pay" [PayCost ctx cost, again yes], label "Decline" [again no]]
        else push (again no)
    Pay cost e -> do
      can <- canPayCost iid cost
      when (playing && can) $ pushAll [PayCost ctx cost, again e]
    May prompt e
      | playing -> chooseFor iid prompt [label prompt [again e], label "Decline" []]
      | otherwise -> pure ()
    Choose options -> do
      affordable <- filterM (optionAffordable . snd) options
      useful <- filterM (effectUseful ctx . snd) affordable
      when playing $ chooseFor iid "Choose one" [label t [again e] | (t, e) <- useful]
    If p yes no -> do
      ok <- evalPredicate ctx p
      push (again (if ok then yes else no))
    GainE g -> when playing $ gain ctx g
    LoseMoney a -> addMoney iid (negate (amt a))
    BuyFromDisplay mtrait half limit ifBought -> when playing $ push (BuyFromDisplayMsg ctx mtrait half limit ifBought)
    DiscardAFocus -> when playing do
      i <- getInvestigator iid
      chooseFor
        iid
        "Discard a focus"
        [Choice (SkillLabel s) [DiscardFocus iid s] | (s, n) <- Map.toList i.focus, n > 0]
    BuyFromDeck kind n limit half -> when playing do
      deck <- use (assetDeckLens kind)
      let (revealed, rest) = splitAt n deck
      assetDeckLens kind .= rest
      logText ("Revealed " <> tshow (length revealed) <> " cards")
      push (BuyRevealed ctx kind revealed limit half 0)
    PlaceCluesOnSheet a -> do
      #sheetClues += amt a
      push CheckStateTriggers
    Focus mskill evenIfExceeds -> when playing do
      i <- getInvestigator iid
      let options = [s | s <- maybe allSkills pure mskill, Map.findWithDefault 0 s i.focus == 0]
      chooseFor
        iid
        "Choose a skill to focus"
        [Choice (SkillLabel s) [FocusSkill iid s evenIfExceeds] | s <- options]
    SufferDamage a -> push (SufferHarm iid ctx.source NormalHarm (amt a) 0)
    SufferHorror a -> push (SufferHarm iid ctx.source NormalHarm 0 (amt a))
    SufferHarmE d h -> push (SufferHarm iid ctx.source NormalHarm (amt d) (amt h))
    DirectDamage a -> push (SufferHarm iid ctx.source DirectHarm (amt a) 0)
    DirectHorror a -> push (SufferHarm iid ctx.source DirectHarm 0 (amt a))
    RecoverHealth r a -> recover ctx r (amt a) 0
    RecoverSanity r a -> recover ctx r 0 (amt a)
    RecoverBoth r h a -> recover ctx r (amt h) (amt a)
    RemoveDoomFrom ScenarioSheet a -> #sheetDoom %= max 0 . subtract (amt a)
    -- only spaces holding doom are worth offering
    RemoveDoomFrom w a ->
      withSpaceWhere ctx w (fmap ((> 0) . (.doom)) . getSpace) (\w' -> RemoveDoomFrom w' a) \sid ->
        [RemoveDoom sid (amt a)]
    PlaceDoomAt ScenarioSheet a -> push (PlaceDoomOnSheet (amt a))
    PlaceDoomAt EachSpaceInYourNeighborhood a -> do
      spaces <- yourNeighborhoodSpaces iid
      push (PlaceDoomInOrder ctx.source (concatMap (replicate (amt a)) spaces))
    PlaceDoomAt w a -> withSpace ctx w (\w' -> PlaceDoomAt w' a) \sid -> [PlaceDoomInOrder ctx.source (replicate (amt a) sid)]
    SpreadDoomOnce -> push SpreadDoom
    SpawnOneClue -> push SpawnClue
    SpawnMonster -> push (SpawnMonsterAt Nothing False)
    SpawnMonsterIn w exhausted ->
      withSpace ctx w (\w' -> SpawnMonsterIn w' exhausted) \sid -> [SpawnMonsterAt (Just sid) exhausted]
    ResolveGateBurst -> push GateBurst
    ReadHeadline -> push (DrawHeadline iid)
    DrawMythosTokens n -> do
      i <- getInvestigator iid
      pushAll (replicate n (DrawMythosToken i.player))
    BecomeDelayed -> when playing $ investigatorL iid . #delayed .= True
    BecomeDevoured -> push (DevourInvestigator iid)
    Retire -> push (RetireInvestigator iid)
    MoveUpTo n -> when playing do
      restricted <- isRestrictedByEngagement iid
      unless restricted $ push (MoveStep (MoveState iid n 0 0 True False))
    MoveUpToIgnoringMonsters n -> when playing $ push (MoveStep (MoveState iid n 0 0 True True))
    MoveDirectlyTo w -> when playing $ withSpace ctx w MoveDirectlyTo \sid -> [MoveDirectly iid sid]
    AddToCodex n -> push (AddArchiveToCodex n)
    FlipArchiveCard n -> push (FlipCodexCard n)
    RemoveFromCodex n -> push (RemoveCodexCard n)
    WinGame -> push WinTheGame
    LoseGame -> push (LoseTheGame "The codex")
    Custom key -> case customEffect key of
      Just f -> f ctx
      Nothing -> logText ("Missing custom effect: " <> key)
 where
  optionAffordable = \case
    Pay cost _ -> canPayCost ctx.investigator cost
    _ -> pure True

countOf :: EffectCtx -> Count -> GameM Int
countOf ctx c = do
  let iid = ctx.investigator
  i <- getInvestigator iid
  case c of
    CluesYouHave -> pure i.clues
    ItemsYouHave -> length <$> matchingAssets iid ItemCard
    SpellsYouHave -> length <$> matchingAssets iid SpellCard
    DoomInYourSpace -> maybe (pure 0) (fmap (.doom) . getSpace) i.space
    MonstersInYourNeighborhood -> do
      spaces <- yourNeighborhoodSpaces iid
      uses #monsters (length . filter ((`elem` spaces) . (.space)) . Map.elems)
    CluesInYourNeighborhood ->
      investigatorNeighborhood iid >>= maybe (pure 0) (fmap (.clues) . getNeighborhood)

fixAmount :: EffectCtx -> Amount -> GameM Amount
fixAmount ctx = \case
  Counted c -> N <$> countOf ctx c
  Half a -> Half <$> fixAmount ctx a
  Diff a b -> Diff <$> fixAmount ctx a <*> fixAmount ctx b
  a -> pure a

-- amounts that count the board are fixed when the effect resolves, not when it was written
fixCounts :: EffectCtx -> Effect -> GameM Effect
fixCounts ctx = \case
  SufferDamage a -> SufferDamage <$> f a
  SufferHorror a -> SufferHorror <$> f a
  SufferHarmE a b -> SufferHarmE <$> f a <*> f b
  DirectDamage a -> DirectDamage <$> f a
  DirectHorror a -> DirectHorror <$> f a
  LoseMoney a -> LoseMoney <$> f a
  RecoverHealth r a -> RecoverHealth r <$> f a
  RecoverSanity r a -> RecoverSanity r <$> f a
  RecoverBoth r a b -> RecoverBoth r <$> f a <*> f b
  RemoveDoomFrom w a -> RemoveDoomFrom w <$> f a
  PlaceDoomAt w a -> PlaceDoomAt w <$> f a
  PlaceCluesOnSheet a -> PlaceCluesOnSheet <$> f a
  GainE (Money a) -> GainE . Money <$> f a
  GainE (Clues a) -> GainE . Clues <$> f a
  GainE (Remnants a) -> GainE . Remnants <$> f a
  e -> pure e
 where
  f = fixAmount ctx

gain :: EffectCtx -> Gain -> GameM ()
gain ctx g = do
  let iid = ctx.investigator
      amt = evalAmount ctx
  case g of
    Money a -> addMoney iid (amt a)
    Clues a -> addClues iid (amt a)
    Remnants a -> addRemnants iid (amt a)
    ClueFromNeighborhood -> do
      msid <- investigatorSpace iid
      for_ msid \sid -> do
        s <- getSpace sid
        case (s.kind, s.neighborhood) of
          (MysterySpace, _) | s.clues > 0 -> do
            spaceL sid . #clues -= 1
            gained
          (_, Just nid) -> do
            n <- getNeighborhood nid
            when (n.clues > 0) do
              neighborhoodL nid . #clues -= 1
              gained
          _ -> pure ()
    AnItem mtrait -> gainItem mtrait Nothing
    AnItemValued mtrait bound -> gainItem mtrait (Just bound)
    AnAlly mtrait -> push (GainItemFromDeck iid AllyDeckKind mtrait Nothing)
    ASpell mtrait -> push (GainItemFromDeck iid SpellDeckKind mtrait Nothing)
    Named n -> push (GainNamedCard iid n)
    Condition c -> push (GainConditionMsg iid c)
 where
  gainItem mtrait mbound = do
    let iid = ctx.investigator
    display <- use (#decks . #display)
    eligible <- filterM (\cid -> itemMatches mtrait mbound cid) display
    chooseFor iid "Gain an item"
      $ [Choice (CardLabel cid) [GainFromDisplay iid cid] | cid <- eligible]
      <> [label "Draw from the item deck" [GainItemFromDeck iid ItemDeckKind mtrait mbound]]
  gained = do
    addClues ctx.investigator 1
    #encounter . _Just . #gainedNeighborhoodClue .= True

recover :: EffectCtx -> Recipient -> Int -> Int -> GameM ()
recover ctx r hp sp = do
  let iid = ctx.investigator
  sid <- investigatorSpace iid
  here <- maybe (pure []) investigatorsAt sid
  (invs, allies) <- recoverTargets ctx r hp sp
  let who =
        [Choice (InvestigatorLabel i) [RecoverInvestigator i hp sp] | i <- invs]
          <> [Choice (CardLabel c) [RecoverAsset c hp sp] | c <- allies]
  case r of
    You -> push (RecoverInvestigator iid hp sp)
    EachInvestigatorInYourSpace -> pushAll [RecoverInvestigator i.id hp sp | i <- here]
    -- only those with something to recover are offered
    _ -> chooseFor iid "Choose who recovers" who

yourNeighborhoodSpaces :: InvestigatorId -> GameM [SpaceId]
yourNeighborhoodSpaces iid = do
  mnid <- investigatorNeighborhood iid
  board <- use #board
  pure $ maybe [] (`neighborhoodSpaces` board) mnid

sourceSpaceOf :: EffectCtx -> GameM (Maybe SpaceId)
sourceSpaceOf ctx = case ctx.source of
  SourceMonster m -> uses #monsters (fmap (.space) . Map.lookup m)
  _ -> investigatorSpace ctx.investigator

scopeInvestigators :: EffectCtx -> InvestigatorScope -> GameM [InvestigatorId]
scopeInvestigators ctx = \case
  EveryInvestigator -> map (.id) <$> playingInvestigators
  NearestToSource ->
    sourceSpaceOf ctx >>= \case
      Nothing -> pure []
      Just from -> do
        invs <- playingInvestigators
        closest <- closestTo from (nub (mapMaybe (.space) invs))
        pure [i.id | i <- invs, maybe False (`elem` closest) i.space]
  InSourceNeighborhood ->
    sourceSpaceOf ctx >>= \case
      Nothing -> pure []
      Just from -> do
        nid <- (.neighborhood) <$> getSpace from
        invs <- playingInvestigators
        let sameHood i = case i.space of
              Nothing -> pure False
              Just sid -> (== nid) . (.neighborhood) <$> getSpace sid
        matching <- filterM' sameHood invs
        pure [i.id | isJust nid, i <- matching]

withSpace :: EffectCtx -> Where -> (Where -> Effect) -> (SpaceId -> [Message]) -> GameM ()
withSpace ctx w = withSpaceWhere ctx w (const (pure True))

-- | 'withSpace', offering only the spaces that pass @keep@
withSpaceWhere
  :: EffectCtx
  -> Where
  -> (SpaceId -> GameM Bool)
  -> (Where -> Effect)
  -> (SpaceId -> [Message])
  -> GameM ()
withSpaceWhere ctx w keep rebuild k = do
  let iid = ctx.investigator
  candidates <-
    filterM keep =<< case w of
      YourSpace -> maybeToList <$> investigatorSpace iid
      SpaceInYourNeighborhood -> yourNeighborhoodSpaces iid
      OtherSpaceInYourNeighborhood -> do
        mine <- investigatorSpace iid
        filter ((/= mine) . Just) <$> yourNeighborhoodSpaces iid
      AnySpace -> allNeighborhoodSpaces
      DifferentSpaces _ excluded -> filter (`notElem` excluded) <$> allNeighborhoodSpaces
      EachSpaceInYourNeighborhood -> yourNeighborhoodSpaces iid
      TheSpace sid -> pure [sid]
      TheUnstableSpace -> unstableSpaces
      AdjacentSpaceWithMostDoom -> do
        board <- use #board
        msid <- case ctx.source of
          SourceMonster m -> uses #monsters (fmap (.space) . Map.lookup m)
          _ -> investigatorSpace iid
        adj <- reachable (maybe [] (`adjacentSpaces` board) msid)
        spaces <- traverse getSpace adj
        pure case spaces of
          [] -> []
          _ -> let best = maximum (map (.doom) spaces) in [s.id | s <- spaces, s.doom == best]
      AdjacentSpace -> do
        board <- use #board
        msid <- case ctx.source of
          SourceMonster m -> uses #monsters (fmap (.space) . Map.lookup m)
          _ -> investigatorSpace iid
        reachable (maybe [] (`adjacentSpaces` board) msid)
      AdjacentStreet -> do
        board <- use #board
        msid <- investigatorSpace iid
        streets <-
          filterM (fmap (isStreetLike . (.kind)) . getSpace) (maybe [] (`adjacentSpaces` board) msid)
        reachable streets
      SourceSpace -> case ctx.source of
        SourceMonster mid -> uses #monsters (maybeToList . fmap (.space) . Map.lookup mid)
        _ -> maybeToList <$> investigatorSpace iid
      ScenarioSheet -> pure []
  case w of
    EachSpaceInYourNeighborhood -> pushAll (concatMap k candidates)
    DifferentSpaces n excluded
      | n <= 0 -> pure ()
      | otherwise ->
          chooseFor iid "Choose a space"
            $ spaceChoices
              candidates
              (\sid -> k sid <> [ResolveEffect ctx (rebuild (DifferentSpaces (n - 1) (sid : excluded)))])
            <> [Choice (DoneLabel "Done") []]
    _ -> case candidates of
      [sid] -> pushAll (k sid)
      _ -> chooseFor iid "Choose a space" (spaceChoices candidates k)

evalPredicate :: EffectCtx -> Predicate -> GameM Bool
evalPredicate ctx p = do
  let iid = ctx.investigator
  i <- getInvestigator iid
  case p of
    HasMoney n -> pure (i.money >= n)
    HasClues n -> pure (i.clues >= n)
    HasRemnants n -> pure (i.remnants >= n)
    HasCondition c -> hasCondition iid c
    HasCard f -> not . null <$> matchingAssets iid f
    IsDelayed -> pure i.delayed
    CodexHas n -> codexHas n
    Not q -> not <$> evalPredicate ctx q
    CustomPredicate key -> do
      logText ("Missing custom predicate: " <> key)
      pure False

payCost :: EffectCtx -> Cost -> GameM ()
payCost ctx cost = do
  let iid = ctx.investigator
  case cost of
    SpendMoney n -> addMoney iid (negate n)
    SpendRemnants n -> addRemnants iid (negate n)
    SpendClues n -> addClues iid (negate n)
    SpendFocus n -> when (n > 0) do
      i <- getInvestigator iid
      chooseFor
        iid
        "Spend a focus"
        [ Choice (SkillLabel s) [DiscardFocus iid s, PayCost ctx (SpendFocus (n - 1))]
        | (s, k) <- Map.toList i.focus
        , k > 0
        ]
    CostDamage n -> push (SufferHarm iid ctx.source NormalHarm n 0)
    CostHorror n -> push (SufferHarm iid ctx.source NormalHarm 0 n)
    CostDelayed -> investigatorL iid . #delayed .= True
    CostCondition c -> push (GainConditionMsg iid c)
    CostDiscard f -> do
      cs <- matchingAssets iid f
      chooseFor iid "Discard a card" [Choice (CardLabel c) [DiscardAsset c] | c <- cs]
    AllOf cs -> pushAll [PayCost ctx c | c <- cs]
