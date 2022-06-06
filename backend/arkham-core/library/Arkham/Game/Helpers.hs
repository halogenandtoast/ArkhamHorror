module Arkham.Game.Helpers
  ( module Arkham.Game.Helpers
  , module X
  ) where

import Arkham.Prelude

import Arkham.Helpers.Ability as X
import Arkham.Helpers.Query as X
import Arkham.Helpers.Scenario as X
import Arkham.Helpers.Window as X
import Arkham.Helpers.Xp as X

import Arkham.Ability
import Arkham.Act.Attrs ( ActAttrs, Field (..) )
import Arkham.Act.Sequence qualified as AS
import Arkham.Action ( Action, TakenAction (..) )
import Arkham.Action qualified as Action
import Arkham.Agenda.Attrs ( AgendaAttrs, Field (..) )
import Arkham.Asset.Attrs ( AssetAttrs, Field (..) )
import Arkham.Asset.Uses ( useCount )
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Card.Cost
import Arkham.Card.EncounterCard
import Arkham.Card.Id
import Arkham.Classes
import Arkham.ClassSymbol
import Arkham.Cost
import Arkham.Criteria ( Criterion )
import Arkham.Criteria qualified as Criteria
import Arkham.DamageEffect
import Arkham.Deck hiding ( InvestigatorDiscard )
import Arkham.Decks
import Arkham.Direction
import Arkham.Effect.Attrs ( EffectAttrs, Field (..) )
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterCard
import Arkham.EncounterSet
import Arkham.Enemy.Attrs ( EnemyAttrs, Field (..) )
import Arkham.Event.Attrs ( EventAttrs, Field (..) )
import Arkham.GameValue
import Arkham.Helpers
import Arkham.History
import Arkham.Id
import Arkham.Investigator.Attrs ( Field (..), InvestigatorAttrs (..) )
import Arkham.Keyword
import Arkham.Keyword qualified as Keyword
import Arkham.Label qualified as Location
import Arkham.Location.Attrs hiding ( location )
import Arkham.Matcher qualified as Matcher
import Arkham.Message hiding ( InvestigatorDamage )
import Arkham.Modifier
import Arkham.Name
import Arkham.Phase
import Arkham.Projection
import Arkham.Scenario.Attrs ( Field (..), ScenarioAttrs )
import Arkham.Scenario.Deck
import Arkham.ScenarioLogKey
import Arkham.Skill.Attrs ( Field (..), SkillAttrs )
import Arkham.SkillTest
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Slot
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait ( Trait (Tome), toTraits )
import Arkham.Treachery.Attrs ( Field (..), TreacheryAttrs )
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet ( size )
import Data.HashSet qualified as HashSet
import Data.UUID ( nil )
import System.IO.Unsafe

gatherEncounterSet :: MonadRandom m => EncounterSet -> m [EncounterCard]
gatherEncounterSet encounterSet = concat <$> for
  defs
  \def -> traverse genEncounterCard
    $ replicate (fromMaybe 0 (cdEncounterSetQuantity def)) def
 where
  defs =
    filter ((== Just encounterSet) . cdEncounterSet) $ toList allEncounterCards

cancelToken :: (HasQueue env, MonadIO m, MonadReader env m) => Token -> m ()
cancelToken token = withQueue $ \queue ->
  ( filter
    (\case
      When (RevealToken _ _ token') | token == token' -> False
      RevealToken _ _ token' | token == token' -> False
      After (RevealToken _ _ token') | token == token' -> False
      RequestedTokens _ _ [token'] | token == token' -> False
      RequestedTokens{} -> error "not setup for multiple tokens"
      _ -> True
    )
    queue
  , ()
  )

replaceToken :: (HasQueue env, MonadIO m, MonadReader env m) => Token -> m ()
replaceToken token = withQueue $ \queue ->
  ( map
    (\case
      When (RevealToken s i _) -> When (RevealToken s i token)
      RevealToken s i _ -> RevealToken s i token
      After (RevealToken s i _) -> After (RevealToken s i token)
      RequestedTokens source' miid [_] -> RequestedTokens source' miid [token]
      RequestedTokens{} -> error "not setup for multiple tokens"
      m -> m
    )
    queue
  , ()
  )

withBaseAbilities :: HasAbilities a => a -> [Ability] -> [Ability]
withBaseAbilities a f = getAbilities a <> f

getPlayableCards
  :: CanCheckWindow env m => InvestigatorAttrs -> CostStatus -> [Window] -> m [Card]
getPlayableCards a@InvestigatorAttrs {..} costStatus windows = do
  asIfInHandCards <- getAsIfInHandCards a
  playableDiscards <- getPlayableDiscards a costStatus windows
  playableHandCards <- filterM
    (getIsPlayable (toId a) (toSource a) costStatus windows)
    (investigatorHand <> asIfInHandCards)
  pure $ playableHandCards <> playableDiscards

getPlayableDiscards
  :: CanCheckWindow env m => InvestigatorAttrs -> CostStatus -> [Window] -> m [Card]
getPlayableDiscards attrs@InvestigatorAttrs {..} costStatus windows = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  filterM
    (getIsPlayable (toId attrs) (toSource attrs) costStatus windows)
    (possibleCards modifiers)
 where
  possibleCards modifiers = map (PlayerCard . snd) $ filter
    (canPlayFromDiscard modifiers)
    (zip @_ @Int [0 ..] investigatorDiscard)
  canPlayFromDiscard modifiers (n, card) =
    cdPlayableFromDiscard (toCardDef card)
      || any (allowsPlayFromDiscard n card) modifiers
  allowsPlayFromDiscard 0 card (CanPlayTopOfDiscard (mcardType, traits)) =
    maybe True (== cdCardType (toCardDef card)) mcardType
      && (null traits
         || (setFromList traits `HashSet.isSubsetOf` toTraits (toCardDef card)
            )
         )
  allowsPlayFromDiscard _ _ _ = False

getAsIfInHandCards :: (Monad m, HasModifiersFor ()) => InvestigatorAttrs -> m [Card]
getAsIfInHandCards attrs = do
  modifiers <- getModifiers (toSource attrs) (toTarget attrs)
  let
    modifiersPermitPlayOfDiscard c =
      any (modifierPermitsPlayOfDiscard c) modifiers
    modifierPermitsPlayOfDiscard (c, depth) = \case
      CanPlayTopOfDiscard (mType, traits) | depth == 0 ->
        maybe True (== toCardType c) mType
          && (null traits || notNull
               (setFromList traits `intersection` toTraits c)
             )
      _ -> False
    modifiersPermitPlayOfDeck c = any (modifierPermitsPlayOfDeck c) modifiers
    modifierPermitsPlayOfDeck (c, depth) = \case
      CanPlayTopOfDeck cardMatcher | depth == 0 -> cardMatch c cardMatcher
      _ -> False
    cardsAddedViaModifiers = flip mapMaybe modifiers $ \case
      AsIfInHand c -> Just c
      _ -> Nothing
  pure
    $ map
        (PlayerCard . fst)
        (filter
          modifiersPermitPlayOfDiscard
          (zip (investigatorDiscard attrs) [0 :: Int ..])
        )
    <> map
         (PlayerCard . fst)
         (filter
           modifiersPermitPlayOfDeck
           (zip (unDeck $ investigatorDeck attrs) [0 :: Int ..])
         )
    <> cardsAddedViaModifiers

getCanPerformAbility
  :: (HasCallStack, CanCheckWindow env m)
  => InvestigatorId
  -> Source
  -> Window
  -> Ability
  -> m Bool
getCanPerformAbility !iid !source !window !ability = do
-- can perform an ability means you can afford it
-- it is in the right window
-- passes restrictions
  let
    mAction = case abilityType ability of
      ActionAbilityWithBefore _ mBeforeAction _ -> mBeforeAction
      _ -> abilityAction ability
    cost = abilityCost ability
  andM
    [ getCanAffordCost iid (abilitySource ability) mAction [window] cost
    , meetsActionRestrictions iid window ability
    , windowMatches iid source window (abilityWindow ability)
    , maybe
      (pure True)
      (passesCriteria iid (abilitySource ability) [window])
      (abilityCriteria ability)
    ]

meetsActionRestrictions
  :: (Monad m, Query Matcher.LocationMatcher m, Query Matcher.EnemyMatcher m)
  => InvestigatorId
  -> Window
  -> Ability
  -> m Bool
meetsActionRestrictions _ _ Ability {..} = go abilityType
 where
  go = \case
    Objective aType -> go aType
    ActionAbilityWithBefore _ mBeforeAction cost ->
      go $ ActionAbility mBeforeAction cost
    ActionAbilityWithSkill mAction _ cost -> go $ ActionAbility mAction cost
    ActionAbility (Just action) _ -> case action of
      Action.Fight -> case abilitySource of
        EnemySource _ -> pure True
        _ -> do
          notNull <$> select Matcher.CanFightEnemy
      Action.Evade -> case abilitySource of
        EnemySource _ -> pure True
        _ -> notNull <$> select Matcher.CanEvadeEnemy
      Action.Engage -> case abilitySource of
        EnemySource _ -> pure True
        _ -> notNull <$> select Matcher.CanEngageEnemy
      Action.Investigate -> case abilitySource of
        LocationSource _ -> pure True
        _ -> notNull <$> select Matcher.InvestigatableLocation
      -- The below actions may not be handled correctly yet
      Action.Ability -> pure True
      Action.Draw -> pure True
      Action.Move -> pure True
      Action.Parley -> pure True
      Action.Play -> pure True
      Action.Resign -> pure True
      Action.Resource -> pure True
    ActionAbility Nothing _ -> pure True
    FastAbility _ -> pure True
    ReactionAbility _ _ -> pure True
    ForcedAbility _ -> pure True
    SilentForcedAbility _ -> pure True
    ForcedAbilityWithCost _ _ -> pure True
    AbilityEffect _ -> pure True

getCanAffordAbility
  :: ( HasModifiersFor ()
     , Monad m
     , HasCallStack
     , Query Matcher.InvestigatorMatcher m
     , Query Matcher.EventMatcher m
     , Query Matcher.AssetMatcher m
     , Projection m InvestigatorAttrs
     , Projection m AssetAttrs
     )
  => InvestigatorId
  -> Ability
  -> Window
  -> m Bool
getCanAffordAbility iid ability window =
  (&&)
    <$> getCanAffordUse iid ability window
    <*> getCanAffordAbilityCost iid ability

getCanAffordAbilityCost
  :: ( HasModifiersFor ()
     , Monad m
     , HasCallStack
     , Query Matcher.InvestigatorMatcher m
     , Query Matcher.EventMatcher m
     , Query Matcher.AssetMatcher m
     , Projection m InvestigatorAttrs
     , Projection m AssetAttrs
     )
  => InvestigatorId
  -> Ability
  -> m Bool
getCanAffordAbilityCost iid Ability {..} = case abilityType of
  ActionAbility mAction cost ->
    getCanAffordCost iid abilitySource mAction [] cost
  ActionAbilityWithSkill mAction _ cost ->
    getCanAffordCost iid abilitySource mAction [] cost
  ActionAbilityWithBefore _ mBeforeAction cost ->
    getCanAffordCost iid abilitySource mBeforeAction [] cost
  ReactionAbility _ cost -> getCanAffordCost iid abilitySource Nothing [] cost
  FastAbility cost -> getCanAffordCost iid abilitySource Nothing [] cost
  ForcedAbilityWithCost _ cost ->
    getCanAffordCost iid abilitySource Nothing [] cost
  ForcedAbility _ -> pure True
  SilentForcedAbility _ -> pure True
  AbilityEffect _ -> pure True
  Objective{} -> pure True

getCanAffordUse
  :: ( Monad m
     , Query Matcher.InvestigatorMatcher m
     , Projection m InvestigatorAttrs
     )
  => InvestigatorId
  -> Ability
  -> Window
  -> m Bool
getCanAffordUse iid ability window = case abilityLimit ability of
  NoLimit -> case abilityType ability of
    ReactionAbility _ _ ->
      notElem ability
        <$> fieldMap InvestigatorUsedAbilities (map usedAbility) iid
    ForcedAbility _ ->
      notElem ability
        <$> fieldMap InvestigatorUsedAbilities (map usedAbility) iid
    SilentForcedAbility _ ->
      notElem ability
        <$> fieldMap InvestigatorUsedAbilities (map usedAbility) iid
    ForcedAbilityWithCost _ _ ->
      notElem ability
        <$> fieldMap InvestigatorUsedAbilities (map usedAbility) iid
    ActionAbility _ _ -> pure True
    ActionAbilityWithBefore{} -> pure True
    ActionAbilityWithSkill{} -> pure True
    FastAbility _ -> pure True
    AbilityEffect _ -> pure True
    Objective{} -> pure True
  PlayerLimit (PerSearch (Just _)) n ->
    (< n)
      . count ((== ability) . usedAbility)
      <$> field InvestigatorUsedAbilities iid
  PlayerLimit _ n ->
    (< n)
      . count ((== ability) . usedAbility)
      <$> field InvestigatorUsedAbilities iid
  PerInvestigatorLimit _ n -> do
    -- This is difficult and based on the window, so we need to match out the
    -- relevant investigator ids from the window. If this becomes more prevalent
    -- we may want a method from `Window -> Maybe InvestigatorId`
    usedAbilities <- field InvestigatorUsedAbilities iid
    case window of
      Window _ (Window.CommittedCards iid' _) -> do
        let
          matchingPerInvestigatorCount =
            flip count usedAbilities $ \usedAbility ->
              case usedAbilityWindow usedAbility of
                Window _ (Window.CommittedCard iid'' _) -> iid' == iid''
                _ -> False
        pure $ matchingPerInvestigatorCount < n
      _ -> error "Unhandled per investigator limit"
  GroupLimit _ n -> do
    usedAbilities <-
      concatMapM (fieldMap InvestigatorUsedAbilities (map usedAbility))
        =<< getInvestigatorIds
    let total = count (== ability) usedAbilities
    pure $ total < n

applyActionCostModifier
  :: [Action] -> Maybe Action -> ModifierType -> Int -> Int
applyActionCostModifier _ (Just action) (ActionCostOf (IsAction action') m) n
  | action == action' = n + m
applyActionCostModifier takenActions (Just action) (ActionCostOf (FirstOneOf as) m) n
  | action `elem` as && all (`notElem` takenActions) as
  = n + m
applyActionCostModifier _ _ (ActionCostModifier m) n = n + m
applyActionCostModifier _ _ _ n = n

getCanAffordCost
  :: ( HasModifiersFor ()
     , Monad m
     , HasCallStack
     , Query Matcher.InvestigatorMatcher m
     , Query Matcher.EventMatcher m
     , Query Matcher.AssetMatcher m
     , Projection m InvestigatorAttrs
     , Projection m AssetAttrs
     )
  => InvestigatorId
  -> Source
  -> Maybe Action
  -> [Window]
  -> Cost
  -> m Bool
getCanAffordCost iid source mAction windows' = \case
  Free -> pure True
  UpTo{} -> pure True
  AdditionalActionsCost{} -> pure True
  Costs xs ->
    and <$> traverse (getCanAffordCost iid source mAction windows') xs
  ExhaustCost target -> case target of
    AssetTarget aid -> do
      readyAssetIds <- selectList Matcher.AssetReady
      pure $ aid `elem` readyAssetIds
    EventTarget eid -> do
      readyEventIds <- selectList Matcher.EventReady
      pure $ eid `elem` readyEventIds
    _ -> error $ "Not handled" <> show target
  ExhaustAssetCost matcher ->
    notNull <$> select (matcher <> Matcher.AssetReady)
  UseCost assetMatcher _uType n -> do
    assets <- selectList assetMatcher
    uses <- sum <$> traverse (fmap useCount . field AssetUses) assets
    pure $ uses >= n
  ActionCost n -> do
    modifiers <- getModifiers source (InvestigatorTarget iid)
    if ActionsAreFree `elem` modifiers
      then pure True
      else do
        takenActions <- field InvestigatorActionsTaken iid
        let
          modifiedActionCost =
            foldr (applyActionCostModifier takenActions mAction) n modifiers
        traits <- case source of
          AssetSource aid -> fieldMap AssetTraits HashSet.toList aid
          _ -> pure []

        tomeActions <- if Tome `elem` traits
          then field InvestigatorTomeActions iid
          else pure 0
        actionCount <- field InvestigatorRemainingActions iid
        pure $ (actionCount + tomeActions) >= modifiedActionCost
  ClueCost n -> do
    spendableClues <- getSpendableClueCount [iid]
    pure $ spendableClues >= n
  PlaceClueOnLocationCost n -> do
    spendableClues <- getSpendableClueCount [iid]
    pure $ spendableClues >= n
  GroupClueCost n locationMatcher -> do
    cost <- getPlayerCountValue n
    iids <- selectList $ Matcher.InvestigatorAt locationMatcher
    totalSpendableClues <- getSpendableClueCount iids
    pure $ totalSpendableClues >= cost
  ResourceCost n -> fieldP InvestigatorResources (>= n) iid
  DiscardFromCost n zone cardMatcher -> do
    -- We need to check that n valid candidates exist across all zones
    -- the logic is that we'll grab all card defs from each zone and then
    -- filter
    let
      getCards = \case
        FromHandOf whoMatcher ->
          fmap (filter (`cardMatch` cardMatcher) . concat)
            . traverse (field InvestigatorHand)
            =<< selectList whoMatcher
        FromPlayAreaOf whoMatcher -> do
          assets <- selectList $ Matcher.AssetControlledBy whoMatcher
          traverse (field AssetCard) assets
        CostZones zs -> concatMapM getCards zs
    (> n) . length <$> getCards zone
  DiscardCost _ -> pure True -- TODO: Make better
  DiscardCardCost _ -> pure True -- TODO: Make better
  DiscardDrawnCardCost -> pure True -- TODO: Make better
  ExileCost _ -> pure True -- TODO: Make better
  RemoveCost _ -> pure True -- TODO: Make better
  HorrorCost{} -> pure True -- TODO: Make better
  DamageCost{} -> pure True -- TODO: Make better
  DirectDamageCost{} -> pure True -- TODO: Make better
  DoomCost{} -> pure True -- TODO: Make better
  SkillIconCost n skillTypes -> do
    handCards <- mapMaybe (preview _PlayerCard) <$> field InvestigatorHand iid
    let
      total = sum $ map
        (count (`member` insertSet SkillWild skillTypes) . cdSkills . toCardDef)
        handCards
    pure $ total >= n
  HandDiscardCost n cardMatcher -> do
    cards <- mapMaybe (preview _PlayerCard) <$> field InvestigatorHand iid
    pure $ length (filter (`cardMatch` cardMatcher) cards) >= n

getActions
  :: ( HasCallStack
     , MonadIO m
     , CanGetAbilities m
     , CanCheckWindow env m
     , Query Matcher.AbilityMatcher m
     , HasModifiersFor ()
     , Monad m
     )
  => InvestigatorId
  -> Window
  -> m [Ability]
getActions iid window = do
  modifiersForFilter <- getModifiers
    (InvestigatorSource iid)
    (InvestigatorTarget iid)
  let
    abilityFilters = mapMaybe
      (\case
        CannotTriggerAbilityMatching m -> Just m
        _ -> Nothing
      )
      modifiersForFilter
  unfilteredActions <- nub <$> getAllAbilities
  actions' <- if null abilityFilters
    then pure unfilteredActions
    else do
      ignored <- select (mconcat abilityFilters)
      pure $ filter (`member` ignored) unfilteredActions
  actionsWithSources <- concat <$> for
    actions'
    \action -> do
      case abilitySource action of
        ProxySource (AssetMatcherSource m) base -> do
          sources <- selectListMap AssetSource m
          pure $ map
            (\source -> action { abilitySource = ProxySource source base })
            sources
        ProxySource (LocationMatcherSource m) base -> do
          sources <- selectListMap LocationSource m
          pure $ map
            (\source -> action { abilitySource = ProxySource source base })
            sources
        _ -> pure [action]

  actions'' <- catMaybes <$> for
    actionsWithSources
    \ability -> do
      modifiers' <- getModifiers
        (InvestigatorSource iid)
        (sourceToTarget $ abilitySource ability)
      investigatorModifiers <- getModifiers
        (InvestigatorSource iid)
        (InvestigatorTarget iid)
      cardClasses <- case abilitySource ability of
        AssetSource aid -> field AssetClasses aid
        _ -> pure $ singleton Neutral
      let
        -- Lola Hayes: Forced abilities will always trigger
        prevents (CanOnlyUseCardsInRole role) =
          null (setFromList [role, Neutral] `intersect` cardClasses)
            && not (isForcedAbility ability)
        prevents CannotTriggerFastAbilities = isFastAbility ability
        prevents _ = False
        -- If the window is fast we only permit fast abilities, but forced
        -- abilities need to be everpresent so we include them
        needsToBeFast = windowType window == Window.FastPlayerWindow && not
          (isFastAbility ability || isForcedAbility ability)
      if any prevents investigatorModifiers || needsToBeFast
        then pure Nothing
        else pure $ Just $ applyAbilityModifiers ability modifiers'
  actions''' <- filterM
    (\action -> liftA2
      (&&)
      (getCanPerformAbility iid (abilitySource action) window action)
      (getCanAffordAbility iid action window)
    )
    actions''
  let forcedActions = filter isForcedAbility actions'''
  pure $ if null forcedActions then actions''' else forcedActions

getHasRecord :: HasRecord m () => CampaignLogKey -> m Bool
getHasRecord k = hasRecord k ()

getRecordCount :: HasRecord m () => CampaignLogKey -> m Int
getRecordCount k = hasRecordCount k ()

getRecordSet :: HasRecord m () => CampaignLogKey -> m [Recorded CardCode]
getRecordSet k = hasRecordSet k ()

getInvestigatorModifiers
  :: (Monad m, HasModifiersFor ()) => InvestigatorId -> Source -> m [ModifierType]
getInvestigatorModifiers iid source =
  getModifiers source (InvestigatorTarget iid)

getPlayerCountValue
  :: (Functor m, Query Matcher.InvestigatorMatcher m) => GameValue Int -> m Int
getPlayerCountValue gameValue = fromGameValue gameValue <$> getPlayerCount

getSpendableClueCount
  :: (Projection m InvestigatorAttrs, Query Matcher.InvestigatorMatcher m)
  => [InvestigatorId]
  -> m Int
getSpendableClueCount investigatorIds =
  selectAgg (+) InvestigatorClues
    $ Matcher.InvestigatorWithoutModifier CannotSpendClues
    <> Matcher.AnyInvestigator (map Matcher.InvestigatorWithId investigatorIds)

-- TODO: canFight _ a@Attrs {..} = canDo Action.Fight a
getCanFight
  :: ( Query Matcher.LocationMatcher m
     , Query Matcher.InvestigatorMatcher m
     , Query Matcher.EventMatcher m
     , Query Matcher.AssetMatcher m
     , HasModifiersFor ()
     , Projection m EnemyAttrs
     , Projection m InvestigatorAttrs
     , Projection m AssetAttrs
     , Monad m
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanFight eid iid = do
  mLocationId <- field InvestigatorLocation iid
  enemyModifiers <- getModifiers (InvestigatorSource iid) (EnemyTarget eid)
  sameLocation <- (isJust mLocationId &&) . (== mLocationId) <$> selectOne
    (Matcher.locationWithEnemy eid)
  modifiers' <- getModifiers (EnemySource eid) (InvestigatorTarget iid)
  takenActions <- fieldMap InvestigatorActionsTaken setFromList iid
  keywords <- field EnemyKeywords eid
  canAffordActions <- getCanAffordCost
    iid
    (EnemySource eid)
    (Just Action.Fight)
    []
    (foldl' (applyFightCostModifiers takenActions) (ActionCost 1) modifiers')
  engagedInvestigators <- field EnemyEngagedInvestigators eid
  pure
    $ canAffordActions
    && (Keyword.Aloof `notMember` keywords || iid `member` engagedInvestigators)
    && (sameLocation || CanBeFoughtAsIfAtYourLocation `elem` enemyModifiers)
 where
  applyFightCostModifiers
    :: HashSet Action.Action -> Cost -> ModifierType -> Cost
  applyFightCostModifiers takenActions costToFight (ActionCostOf actionTarget n)
    = case actionTarget of
      FirstOneOf as
        | Action.Fight `elem` as && null
          (takenActions `intersect` setFromList as)
        -> increaseActionCost costToFight n
      IsAction Action.Fight -> increaseActionCost costToFight n
      _ -> costToFight
  applyFightCostModifiers _ costToFight _ = costToFight

getCanEngage
  :: ( Query Matcher.LocationMatcher m
     , Query Matcher.InvestigatorMatcher m
     , Query Matcher.EventMatcher m
     , Query Matcher.AssetMatcher m
     , HasModifiersFor ()
     , Projection m EnemyAttrs
     , Projection m InvestigatorAttrs
     , Projection m AssetAttrs
     , Monad m
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanEngage eid iid = do
  mLocationId <- field InvestigatorLocation iid
  sameLocation <- (isJust mLocationId &&) . (== mLocationId) <$> selectOne
    (Matcher.locationWithEnemy eid)
  notEngaged <- notElem iid <$> field EnemyEngagedInvestigators eid
  canAffordActions <- getCanAffordCost
    iid
    (EnemySource eid)
    (Just Action.Engage)
    []
    (ActionCost 1)
  pure $ notEngaged && canAffordActions && sameLocation

getCanEvade
  :: ( HasModifiersFor ()
     , Projection m InvestigatorAttrs
     , Projection m EnemyAttrs
     , Projection m AssetAttrs
     , Query Matcher.InvestigatorMatcher m
     , Query Matcher.EventMatcher m
     , Query Matcher.AssetMatcher m
     , Monad m
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanEvade eid iid = do
  engaged <- elem iid <$> field EnemyEngagedInvestigators eid
  enemyModifiers <- getModifiers (InvestigatorSource iid) (EnemyTarget eid)
  modifiers' <- getModifiers (EnemySource eid) (InvestigatorTarget iid)
  takenActions <- fieldMap InvestigatorActionsTaken setFromList iid
  canAffordActions <- getCanAffordCost
    iid
    (EnemySource eid)
    (Just Action.Evade)
    []
    (foldl' (applyEvadeCostModifiers takenActions) (ActionCost 1) modifiers')
  pure $ engaged && canAffordActions && CannotBeEvaded `notElem` enemyModifiers
 where
  applyEvadeCostModifiers
    :: HashSet Action.Action -> Cost -> ModifierType -> Cost
  applyEvadeCostModifiers takenActions costToFight (ActionCostOf actionTarget n)
    = case actionTarget of
      FirstOneOf as
        | Action.Evade `elem` as && null
          (takenActions `intersect` setFromList as)
        -> increaseActionCost costToFight n
      IsAction Action.Evade -> increaseActionCost costToFight n
      _ -> costToFight
  applyEvadeCostModifiers _ costToFight _ = costToFight

getCanMoveTo
  :: ( HasModifiersFor ()
     , HasHistory m
     , HasCallStack
     , Query Matcher.LocationMatcher m
     , Query Matcher.InvestigatorMatcher m
     , Query Matcher.EventMatcher m
     , Query Matcher.AssetMatcher m
     , Projection m InvestigatorAttrs
     , Projection m AssetAttrs
     , Monad m
     )
  => LocationId
  -> InvestigatorId
  -> m Bool
getCanMoveTo lid iid = do
  history <- getHistory TurnHistory iid
  mLocationId <- field InvestigatorLocation iid
  case mLocationId of
    Nothing -> pure False
    Just locationId -> do
      modifiers' <- getModifiers (LocationSource lid) (InvestigatorTarget iid)
      locationModifiers' <- getModifiers
        (InvestigatorSource iid)
        (LocationTarget lid)
      accessibleLocations <-
        selectList $ Matcher.AccessibleFrom $ Matcher.LocationWithId locationId
      canAffordActions <- getCanAffordCost
        iid
        (LocationSource lid)
        (Just Action.Move)
        []
        (ActionCost 1)
      pure
        $ (lid `elem` accessibleLocations)
        && canAffordActions
        && (lid /= locationId)
        && (CannotMove `notElem` modifiers')
        && (CannotMoveMoreThanOnceEachTurn `notElem` modifiers' || not
             (historyMoved history)
           )
        && (Blocked `notElem` locationModifiers')

toModifier :: SourceEntity a => a -> ModifierType -> Modifier
toModifier = Modifier . toSource

toModifiers :: SourceEntity a => a -> [ModifierType] -> [Modifier]
toModifiers = map . toModifier

targetToSource :: Target -> Source
targetToSource = \case
  InvestigatorTarget iid -> InvestigatorSource iid
  InvestigatorHandTarget iid -> InvestigatorSource iid
  InvestigatorDiscardTarget iid -> InvestigatorSource iid
  AssetTarget aid -> AssetSource aid
  EnemyTarget eid -> EnemySource eid
  ScenarioTarget sid -> ScenarioSource sid
  EffectTarget eid -> EffectSource eid
  PhaseTarget _ -> error "no need"
  LocationTarget lid -> LocationSource lid
  (SetAsideLocationsTarget _) -> error "can not convert"
  SkillTestTarget -> error "can not convert"
  AfterSkillTestTarget -> AfterSkillTestSource
  TreacheryTarget tid -> TreacherySource tid
  EncounterDeckTarget -> error "can not covert"
  ScenarioDeckTarget -> error "can not covert"
  AgendaTarget aid -> AgendaSource aid
  ActTarget aid -> ActSource aid
  CardIdTarget _ -> error "can not convert"
  CardCodeTarget _ -> error "can not convert"
  SearchedCardTarget _ -> error "can not convert"
  EventTarget eid -> EventSource eid
  SkillTarget sid -> SkillSource sid
  SkillTestInitiatorTarget _ -> error "can not convert"
  TokenTarget tid -> TokenSource tid
  TokenFaceTarget _ -> error "Not convertable"
  TestTarget -> TestSource mempty
  ResourceTarget -> ResourceSource
  ActDeckTarget -> ActDeckSource
  AgendaDeckTarget -> AgendaDeckSource
  InvestigationTarget{} -> error "not converted"
  YouTarget -> YouSource
  ProxyTarget{} -> error "can not convert"
  CardTarget{} -> error "can not convert"
  StoryTarget code -> StorySource code

sourceToTarget :: Source -> Target
sourceToTarget = \case
  YouSource -> YouTarget
  AssetSource aid -> AssetTarget aid
  EnemySource eid -> EnemyTarget eid
  CardIdSource cid -> CardIdTarget cid
  ScenarioSource sid -> ScenarioTarget sid
  InvestigatorSource iid -> InvestigatorTarget iid
  CardCodeSource cid -> CardCodeTarget cid
  TokenSource t -> TokenTarget t
  TokenEffectSource _ -> error "not implemented"
  AgendaSource aid -> AgendaTarget aid
  LocationSource lid -> LocationTarget lid
  SkillTestSource{} -> SkillTestTarget
  AfterSkillTestSource -> AfterSkillTestTarget
  TreacherySource tid -> TreacheryTarget tid
  EventSource eid -> EventTarget eid
  SkillSource sid -> SkillTarget sid
  EmptyDeckSource -> error "not implemented"
  DeckSource -> error "not implemented"
  GameSource -> error "not implemented"
  ActSource aid -> ActTarget aid
  PlayerCardSource _ -> error "not implemented"
  EncounterCardSource _ -> error "not implemented"
  TestSource{} -> TestTarget
  ProxySource _ source -> sourceToTarget source
  EffectSource eid -> EffectTarget eid
  ResourceSource -> ResourceTarget
  AbilitySource{} -> error "not implemented"
  ActDeckSource -> ActDeckTarget
  AgendaDeckSource -> AgendaDeckTarget
  AssetMatcherSource{} -> error "not converted"
  LocationMatcherSource{} -> error "not converted"
  EnemyAttackSource a -> EnemyTarget a
  StorySource code -> StoryTarget code

addCampaignCardToDeckChoice
  :: InvestigatorId -> [InvestigatorId] -> CardDef -> Message
addCampaignCardToDeckChoice leadInvestigatorId investigatorIds cardDef =
  chooseOne
    leadInvestigatorId
    [ Label
      ("Add " <> display name <> " to a deck")
      [ chooseOne
          leadInvestigatorId
          [ TargetLabel
              (InvestigatorTarget iid)
              [AddCampaignCardToDeck iid cardDef]
          | iid <- investigatorIds
          ]
      ]
    , Label ("Do not add " <> display name <> " to any deck") []
    ]
  where name = cdName cardDef

skillTestModifier
  :: (SourceEntity source, TargetEntity target)
  => source
  -> target
  -> ModifierType
  -> Message
skillTestModifier source target modifier =
  skillTestModifiers source target [modifier]

skillTestModifiers
  :: (SourceEntity source, TargetEntity target)
  => source
  -> target
  -> [ModifierType]
  -> Message
skillTestModifiers source target modifiers = CreateWindowModifierEffect
  EffectSkillTestWindow
  (EffectModifiers $ toModifiers source modifiers)
  (toSource source)
  (toTarget target)

getJustLocationIdByName
  :: Query Matcher.LocationMatcher m => Name -> m LocationId
getJustLocationIdByName name =
  fromJustNote ("Missing " <> show name) <$> getLocationIdByName name

getLocationIdByName
  :: Query Matcher.LocationMatcher m => Name -> m (Maybe LocationId)
getLocationIdByName name = selectOne matcher
 where
  matcher = case (nameTitle name, nameSubtitle name) of
    (title, Just subtitle) -> Matcher.LocationWithFullTitle title subtitle
    (title, Nothing) -> Matcher.LocationWithTitle title

fightAction :: SourceEntity source => source -> Int -> [Cost] -> Ability
fightAction source n costs =
  mkAbility source n (ActionAbility (Just Action.Fight) (Costs costs))

hasFightActions
  :: Query Matcher.AbilityMatcher m
  => InvestigatorId
  -> Matcher.WindowMatcher
  -> m Bool
hasFightActions _ window = notNull <$> select
  (Matcher.AbilityIsAction Action.Fight <> Matcher.AbilityWindow window)

hasEvadeActions
  :: Query Matcher.AbilityMatcher m
  => InvestigatorId
  -> Matcher.WindowMatcher
  -> m Bool
hasEvadeActions _ window = notNull <$> select
  (Matcher.AbilityIsAction Action.Evade <> Matcher.AbilityWindow window)

hasInvestigateActions
  :: Query Matcher.AbilityMatcher m
  => InvestigatorId
  -> Matcher.WindowMatcher
  -> m Bool
hasInvestigateActions _ window = notNull <$> select
  (Matcher.AbilityIsAction Action.Investigate <> Matcher.AbilityWindow window)

getIsPlayable
  :: CanCheckWindow env m
  => InvestigatorId
  -> Source
  -> CostStatus
  -> [Window]
  -> Card
  -> m Bool
getIsPlayable iid source costStatus windows' c = do
  availableResources <- field InvestigatorResources iid
  getIsPlayableWithResources iid source availableResources costStatus windows' c

getIsPlayableWithResources
  :: CanCheckWindow env m
  => InvestigatorId
  -> Source
  -> Int
  -> CostStatus
  -> [Window]
  -> Card
  -> m Bool
getIsPlayableWithResources _ _ _ _ _ (EncounterCard _) = pure False -- TODO: there might be some playable ones?
getIsPlayableWithResources iid source availableResources costStatus windows' c@(PlayerCard _)
  = do
    iids <- filter (/= iid) <$> getInvestigatorIds
    iidsWithModifiers <- for iids $ \iid' -> do
      modifiers <- getModifiers
        (InvestigatorSource iid')
        (InvestigatorTarget iid')
      pure (iid', modifiers)
    canHelpPay <- flip filterM iidsWithModifiers $ \(_, modifiers) -> do
      flip anyM modifiers $ \case
        CanSpendResourcesOnCardFromInvestigator iMatcher cMatcher -> liftA2
          (&&)
          (member iid <$> select iMatcher)
          (pure $ cardMatch c cMatcher)
        _ -> pure False
    additionalResources <-
      sum <$> traverse (field InvestigatorResources . fst) canHelpPay
    modifiers <- getModifiers (InvestigatorSource iid) (InvestigatorTarget iid)
    let title = nameTitle (cdName pcDef)
    passesUnique <- case (cdUnique pcDef, cdCardType pcDef) of
      (True, AssetType) -> not <$> case nameSubtitle (cdName pcDef) of
        Nothing -> selectAny (Matcher.AssetWithTitle title)
        Just subtitle -> selectAny (Matcher.AssetWithFullTitle title subtitle)
      _ -> pure True
    let
      duringTurnWindow = Window Timing.When (Window.DuringTurn iid)
      notFastWindow = any (`elem` windows') [duringTurnWindow]
      canBecomeFast = foldr applyModifier False modifiers
      canBecomeFastWindow =
        if canBecomeFast then Just (Matcher.DuringTurn Matcher.You) else Nothing
      applyModifier (CanBecomeFast (mcardType, traits)) _
        | maybe True (== cdCardType pcDef) mcardType
          && notNull (setFromList traits `intersect` toTraits pcDef)
        = True
      applyModifier _ val = val
    modifiedCardCost <- getModifiedCardCost iid c
    passesCriterias <- maybe
      (pure True)
      (passesCriteria iid source windows')
      (cdCriteria pcDef)
    inFastWindow <- maybe
      (pure False)
      (cardInFastWindows iid source c windows')
      (cdFastWindow pcDef <|> canBecomeFastWindow)
    canEvade <- hasEvadeActions iid (Matcher.DuringTurn Matcher.You)
    canFight <- hasFightActions iid (Matcher.DuringTurn Matcher.You)
    passesLimits <- allM passesLimit (cdLimits pcDef)
    cardModifiers <- getModifiers
      (CardIdSource $ toCardId c)
      (CardIdTarget $ toCardId c)
    let
      additionalCosts = flip mapMaybe cardModifiers $ \case
        AdditionalCost x -> Just x
        _ -> Nothing

    canAffordAdditionalCosts <- allM
      (getCanAffordCost iid (CardIdSource $ toCardId c) Nothing windows')
      additionalCosts

    passesSlots <- if null (cdSlots pcDef)
      then pure True
      else do
        possibleSlots <- getPotentialSlots (cdCardTraits pcDef) iid
        pure $ null $ cdSlots pcDef \\ possibleSlots

    pure
      $ (cdCardType pcDef /= SkillType)
      && ((costStatus == PaidCost)
         || (modifiedCardCost <= (availableResources + additionalResources))
         )
      && none prevents modifiers
      && ((isNothing (cdFastWindow pcDef) && notFastWindow) || inFastWindow)
      && (cdAction pcDef /= Just Action.Evade || canEvade)
      && (cdAction pcDef /= Just Action.Fight || canFight)
      && passesCriterias
      && passesLimits
      && passesUnique
      && passesSlots
      && canAffordAdditionalCosts
 where
  pcDef = toCardDef c
  prevents (CanOnlyUseCardsInRole role) =
    cdClassSymbol pcDef `notElem` [Just Neutral, Just role, Nothing]
  prevents (CannotPlay typePairs) = any
    (\(cType, traits) ->
      cdCardType pcDef
        == cType
        && (null traits || notNull (intersection (toTraits pcDef) traits))
    )
    typePairs
  prevents _ = False
  passesLimit (LimitPerInvestigator m) = case toCardType c of
    AssetType -> do
      n <- selectCount
        (Matcher.AssetControlledBy (Matcher.InvestigatorWithId iid)
        <> Matcher.AssetWithTitle (nameTitle $ toName c)
        )
      pure $ m > n
    _ -> error $ "Not handling card type: " <> show (toCardType c)
  passesLimit (LimitPerTrait t m) = case toCardType c of
    AssetType -> do
      n <- selectCount (Matcher.AssetWithTrait t)
      pure $ m > n
    _ -> error $ "Not handling card type: " <> show (toCardType c)

passesCriteria
  :: CanCheckWindow env m
  => InvestigatorId
  -> Source
  -> [Window]
  -> Criterion
  -> m Bool
passesCriteria iid source windows' = \case
  Criteria.DoomCountIs valueMatcher -> do
    doomCount <- getDoomCount
    gameValueMatches doomCount valueMatcher
  Criteria.Negate restriction ->
    not <$> passesCriteria iid source windows' restriction
  Criteria.AllUndefeatedInvestigatorsResigned ->
    null <$> select Matcher.UneliminatedInvestigator
  Criteria.EachUndefeatedInvestigator investigatorMatcher -> do
    liftA2
      (==)
      (select Matcher.UneliminatedInvestigator)
      (select investigatorMatcher)
  Criteria.Never -> pure False
  Criteria.InYourHand -> do
    hand <- fieldMap InvestigatorHand (map toCardId) iid
    case source of
      EventSource eid -> pure $ unEventId eid `elem` hand
      TreacherySource tid -> do
        member tid <$> select
          (Matcher.TreacheryInHandOf $ Matcher.InvestigatorWithId iid)
      _ -> error $ "source not handled for in your hand: " <> show source
  Criteria.InThreatAreaOf who -> do
    case source of
      TreacherySource tid ->
        member tid <$> select (Matcher.TreacheryInThreatAreaOf who)
      _ ->
        error
          $ "Can not check if "
          <> show source
          <> " is in players threat area"
  Criteria.Self -> case source of
    InvestigatorSource iid' -> pure $ iid == iid'
    _ -> pure False
  Criteria.ValueIs val valueMatcher -> gameValueMatches val valueMatcher
  Criteria.UnderneathCardCount valueMatcher zone cardMatcher -> do
    let
      getCards = \case
        Criteria.UnderAgendaDeck -> scenarioField ScenarioCardsUnderAgendaDeck
        Criteria.UnderActDeck -> scenarioField ScenarioCardsUnderActDeck
        Criteria.UnderZones zs -> concatMapM getCards zs
    cardCount <- length . filter (`cardMatch` cardMatcher) <$> getCards zone
    gameValueMatches cardCount valueMatcher
  Criteria.SelfHasModifier modifier -> case source of
    InvestigatorSource iid' ->
      elem modifier <$> getModifiers source (InvestigatorTarget iid')
    _ -> pure False
  Criteria.Here -> case source of
    LocationSource lid -> fieldP InvestigatorLocation (== Just lid) iid
    ProxySource (LocationSource lid) _ ->
      fieldP InvestigatorLocation (== Just lid) iid
    _ -> pure False
  Criteria.OwnsThis -> case source of
    AssetSource aid -> member aid
      <$> select (Matcher.AssetControlledBy $ Matcher.InvestigatorWithId iid)
    EventSource eid -> member eid
      <$> select (Matcher.EventControlledBy $ Matcher.InvestigatorWithId iid)
    _ -> pure False
  Criteria.DuringSkillTest skillTestMatcher -> do
    mSkillTest <- getSkillTest
    case mSkillTest of
      Nothing -> pure False
      Just skillTest -> skillTestMatches iid source skillTest skillTestMatcher
  Criteria.ChargesOnThis valueMatcher -> case source of
    TreacherySource tid ->
      (`gameValueMatches` valueMatcher) =<< field TreacheryResources tid
    _ -> error "missing ChargesOnThis check"
  Criteria.ResourcesOnThis valueMatcher -> case source of
    TreacherySource tid ->
      (`gameValueMatches` valueMatcher) =<< field TreacheryResources tid
    _ -> error "missing ChargesOnThis check"
  Criteria.ResourcesOnLocation locationMatcher valueMatcher -> do
    total <- selectAgg (+) LocationResources locationMatcher
    gameValueMatches total valueMatcher
  Criteria.CluesOnThis valueMatcher -> case source of
    LocationSource lid ->
      (`gameValueMatches` valueMatcher) =<< field LocationClues lid
    ActSource aid -> (`gameValueMatches` valueMatcher) =<< field ActClues aid
    AssetSource aid ->
      (`gameValueMatches` valueMatcher) =<< field AssetClues aid
    TreacherySource tid ->
      (`gameValueMatches` valueMatcher) =<< field TreacheryClues tid
    _ -> error "missing CluesOnThis check"
  Criteria.HorrorOnThis valueMatcher -> case source of
    AssetSource aid ->
      (`gameValueMatches` valueMatcher) =<< field AssetHorror aid
    _ -> error $ "missing HorrorOnThis check for " <> show source
  Criteria.ScenarioDeckWithCard key ->
    notNull <$> scenarioFieldMap ScenarioDecks (HashMap.findWithDefault [] key)
  Criteria.Unowned -> case source of
    AssetSource aid -> fieldP AssetController isNothing aid
    ProxySource (AssetSource aid) _ -> fieldP AssetController isNothing aid
    _ -> error $ "missing OwnsThis check for source: " <> show source
  Criteria.OnSameLocation -> case source of
    AssetSource aid ->
      liftA2 (==) (field AssetLocation aid) (field InvestigatorLocation iid)
    EnemySource eid -> liftA2
      (==)
      (selectOne $ Matcher.LocationWithEnemy $ Matcher.EnemyWithId eid)
      (field InvestigatorLocation iid)
    TreacherySource tid -> field TreacheryAttachedTarget tid >>= \case
      Just (LocationTarget lid) ->
        fieldP InvestigatorLocation (== Just lid) iid
      Just _ -> pure False
      Nothing -> pure False
    ProxySource (AssetSource aid) _ ->
      liftA2 (==) (field AssetLocation aid) (field InvestigatorLocation iid)
    _ -> error $ "missing OnSameLocation check for source: " <> show source
  Criteria.DuringTurn who -> do
    iids <- select who
    pure
      $ any ((`elem` windows') . Window Timing.When . Window.DuringTurn) iids
      || (iid
         `elem` iids
         && Window Timing.When Window.FastPlayerWindow
         `elem` windows'
         )
  Criteria.CardExists cardMatcher -> selectAny cardMatcher
  Criteria.ExtendedCardExists cardMatcher -> selectAny cardMatcher
  Criteria.PlayableCardExistsWithCostReduction n cardMatcher -> do
    availableResources <- field InvestigatorResources iid
    results <- selectList cardMatcher
    anyM
      (getIsPlayableWithResources
        iid
        source
        (availableResources + n)
        UnpaidCost
        windows'
      )
      results
  Criteria.PlayableCardExists cardMatcher -> do
    results <- selectList cardMatcher
    anyM (getIsPlayable iid source UnpaidCost windows') results
  Criteria.PlayableCardInDiscard discardSignifier cardMatcher -> do
    let
      investigatorMatcher = case discardSignifier of
        Criteria.DiscardOf matcher -> matcher
        Criteria.AnyPlayerDiscard -> Matcher.Anyone
      windows'' =
        [ Window Timing.When (Window.DuringTurn iid)
        , Window Timing.When Window.FastPlayerWindow
        ]
    investigatorIds <-
      filterM
          (fmap (notElem CardsCannotLeaveYourDiscardPile)
          . getModifiers GameSource
          . InvestigatorTarget
          )
        =<< selectList investigatorMatcher
    discards <-
      filter (`cardMatch` cardMatcher)
        <$> concatMapM (field InvestigatorDiscard) investigatorIds
    anyM (getIsPlayable iid source UnpaidCost windows'' . PlayerCard) discards
  Criteria.FirstAction -> fieldP InvestigatorActionsTaken null iid
  Criteria.NoRestriction -> pure True
  Criteria.OnLocation locationMatcher -> do
    mlid <- field InvestigatorLocation iid
    case mlid of
      Nothing -> pure False
      Just lid -> anyM
        (\window -> locationMatches iid source window lid locationMatcher)
        windows'
  Criteria.ReturnableCardInDiscard discardSignifier traits -> do
    let
      investigatorMatcher = case discardSignifier of
        Criteria.DiscardOf matcher -> matcher
        Criteria.AnyPlayerDiscard -> Matcher.Anyone
    investigatorIds <-
      filterM
          (fmap (notElem CardsCannotLeaveYourDiscardPile)
          . getModifiers GameSource
          . InvestigatorTarget
          )
        =<< selectList investigatorMatcher
    discards <- concatMapM (field InvestigatorDiscard) investigatorIds
    let
      filteredDiscards = case traits of
        [] -> discards
        traitsToMatch ->
          filter (any (`elem` traitsToMatch) . toTraits) discards
    pure $ notNull filteredDiscards
  Criteria.CardInDiscard discardSignifier cardMatcher -> do
    let
      investigatorMatcher = case discardSignifier of
        Criteria.DiscardOf matcher -> matcher
        Criteria.AnyPlayerDiscard -> Matcher.Anyone
    investigatorIds <- selectList investigatorMatcher
    discards <- concatMapM (field InvestigatorDiscard) investigatorIds
    let filteredDiscards = filter (`cardMatch` cardMatcher) discards
    pure $ notNull filteredDiscards
  Criteria.ClueOnLocation ->
    maybe (pure False) (fieldP LocationClues (> 0))
      =<< field InvestigatorLocation iid
  Criteria.EnemyCriteria enemyCriteria ->
    passesEnemyCriteria iid source windows' enemyCriteria
  Criteria.SetAsideCardExists matcher -> selectAny matcher
  Criteria.OnAct step -> do
    actId <- selectJust Matcher.AnyAct
    (== ActStep step) . AS.actStep <$> field ActSequence actId
  Criteria.AssetExists matcher -> notNull <$> select matcher
  Criteria.TreacheryExists matcher -> notNull <$> select matcher
  Criteria.InvestigatorExists matcher ->
    -- Because the matcher can't tell who is asking, we need to replace
    -- The You matcher by the Id of the investigator asking
    notNull <$> select (Matcher.replaceYouMatcher iid matcher)
  Criteria.InvestigatorsHaveSpendableClues valueMatcher -> do
    total <- selectAgg (+) InvestigatorClues
      $ Matcher.InvestigatorWithoutModifier CannotSpendClues
    total `gameValueMatches` valueMatcher
  Criteria.Criteria rs -> allM (passesCriteria iid source windows') rs
  Criteria.AnyCriterion rs -> anyM (passesCriteria iid source windows') rs
  Criteria.LocationExists matcher -> notNull <$> select matcher
  Criteria.InvestigatorIsAlone ->
    (== 1) <$> selectCount (Matcher.colocatedWith iid)
  Criteria.InVictoryDisplay cardMatcher valueMatcher -> do
    vCards <- filter (`cardMatch` cardMatcher)
      <$> scenarioField ScenarioVictoryDisplay
    gameValueMatches (length vCards) valueMatcher
  Criteria.OwnCardWithDoom -> do
    anyAssetsHaveDoom <- selectAny
      (Matcher.AssetControlledBy Matcher.You <> Matcher.AssetWithAnyDoom)
    investigatorHasDoom <- fieldP InvestigatorDoom (> 0) iid
    pure $ investigatorHasDoom || anyAssetsHaveDoom
  Criteria.ScenarioCardHasResignAbility -> do
    actions' <- getAllAbilities
    pure $ flip
      any
      actions'
      \ability -> case abilityType ability of
        ActionAbility (Just Action.Resign) _ -> True
        _ -> False
  Criteria.Remembered rememberedListMatcher logKeys -> do
    filtered <-
      filter (`elem` logKeys)
        <$> scenarioFieldMap ScenarioRemembered HashSet.toList
    rememberedListMatches filtered rememberedListMatcher

-- | Build a matcher and check the list
passesEnemyCriteria
  :: (Projection m EventAttrs, Query Matcher.EnemyMatcher m)
  => InvestigatorId
  -> Source
  -> [Window]
  -> Criteria.EnemyCriterion
  -> m Bool
passesEnemyCriteria _iid source windows' criterion = selectAny
  =<< matcher criterion
 where
  matcher = \case
    Criteria.EnemyMatchesCriteria ms -> mconcatMapM matcher ms
    Criteria.EnemyExists m -> pure m
    Criteria.EnemyExistsAtAttachedLocation m -> case source of
      EventSource e -> do
        field EventAttachedTarget e >>= \case
          Just (LocationTarget lid) ->
            pure $ m <> Matcher.EnemyAt (Matcher.LocationWithId lid)
          Just _ -> error "Event must be attached to a location"
          Nothing -> error "Event must be attached to a location"
      _ -> error $ "Does not handle source: " <> show source
    Criteria.ThisEnemy enemyMatcher -> case source of
      EnemySource eid -> pure $ Matcher.EnemyWithId eid <> enemyMatcher
      _ -> error "Invalid source for ThisEnemy"
    Criteria.NotAttackingEnemy ->
      -- TODO: should not be multiple enemies, but if so need to OR not AND matcher
      let
        getAttackingEnemy = \case
          Window _ (Window.EnemyAttacks _ eid _) -> Just eid
          _ -> Nothing
      in
        case mapMaybe getAttackingEnemy windows' of
          [] -> error "can not be called without enemy source"
          xs -> pure $ Matcher.NotEnemy (concatMap Matcher.EnemyWithId xs)

getModifiedCardCost
  :: (HasModifiersFor (), Query Matcher.CardMatcher m, Monad m)
  => InvestigatorId
  -> Card
  -> m Int
getModifiedCardCost iid c@(PlayerCard _) = do
  modifiers <- getModifiers (InvestigatorSource iid) (InvestigatorTarget iid)
  cardModifiers <- getModifiers
    (InvestigatorSource iid)
    (CardIdTarget $ toCardId c)
  foldM applyModifier startingCost (modifiers <> cardModifiers)
 where
  pcDef = toCardDef c
  startingCost = case cdCost pcDef of
    Just (StaticCost n) -> n
    Just DynamicCost -> 0
    Nothing -> 0
  applyModifier n (ReduceCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n (IncreaseCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then n + m else n
  applyModifier n _ = pure n
getModifiedCardCost iid c@(EncounterCard _) = do
  modifiers <- getModifiers (InvestigatorSource iid) (InvestigatorTarget iid)
  foldM
    applyModifier
    (error "we need so specify ecCost for this to work")
    modifiers
 where
  applyModifier n (ReduceCostOf cardMatcher m) = do
    pure $ if c `cardMatch` cardMatcher then max 0 (n - m) else n
  applyModifier n (IncreaseCostOf cardMatcher m) = do
    matches <- selectList cardMatcher
    pure $ if c `cardMatch` cardMatcher then n + m else n
  applyModifier n _ = pure n

depthGuard :: IORef Int
depthGuard = unsafePerformIO $ newIORef 0
{-# NOINLINE depthGuard #-}

cardInFastWindows
  :: CanCheckWindow env m
  => InvestigatorId
  -> Source
  -> Card
  -> [Window]
  -> Matcher.WindowMatcher
  -> m Bool
cardInFastWindows iid source _ windows' matcher =
  anyM (\window -> windowMatches iid source window matcher) windows'

withDepthLock
  :: (MonadIO m, HasDepth env, MonadReader env m) => Int -> a -> m a -> m a
withDepthLock d defaultValue action = do
  depth <- getDepth
  if depth >= d
    then pure defaultValue
    else do
      delveDeeper
      result <- action
      resurface
      pure result

getDepth :: (HasDepth env, MonadIO m, MonadReader env m) => m Int
getDepth = readIORef =<< view depthL

delveDeeper :: (HasDepth env, MonadIO m, MonadReader env m) => m ()
delveDeeper = do
  ref <- view depthL
  current <- readIORef ref
  writeIORef ref (current + 1)

resurface :: (HasDepth env, MonadIO m, MonadReader env m) => m ()
resurface = do
  ref <- view depthL
  current <- readIORef ref
  writeIORef ref (max 0 (current - 1))

type CanCheckWindow env m
  = ( Projection m AssetAttrs
    , Projection m EnemyAttrs
    , Projection m EffectAttrs
    , Projection m AgendaAttrs
    , Projection m ActAttrs
    , Projection m EventAttrs
    , Projection m InvestigatorAttrs
    , Projection m LocationAttrs
    , Projection m SkillAttrs
    , Projection m TreacheryAttrs
    , Projection m ScenarioAttrs
    , HasModifiersFor ()
    , MonadIO m
    , HasDepth env
    , MonadReader env m
    , Query Matcher.AbilityMatcher m
    , Query Matcher.ScenarioMatcher m
    , Query Matcher.ActMatcher m
    , Query Matcher.InvestigatorMatcher m
    , Query Matcher.EffectMatcher m
    , Query Matcher.CardMatcher m
    , Query Matcher.EnemyMatcher m
    , Query Matcher.EventMatcher m
    , Query Matcher.LocationMatcher m
    , Query Matcher.AssetMatcher m
    , Query Matcher.AgendaMatcher m
    , Query Matcher.TreacheryMatcher m
    , Query Matcher.ExtendedCardMatcher m
    , Query Matcher.SkillMatcher m
    , HasSkillTest m
    , HasTokenValue m ()
    )

windowMatches
  :: CanCheckWindow env m
  => InvestigatorId
  -> Source
  -> Window
  -> Matcher.WindowMatcher
  -> m Bool
windowMatches iid source window' = \case
  Matcher.AnyWindow -> pure True
  Matcher.DrawingStartingHand timing whoMatcher -> case window' of
    Window t (Window.DrawingStartingHand who) | t == timing ->
      matchWho who whoMatcher
    _ -> pure False
  Matcher.MovedFromHunter timing enemyMatcher -> case window' of
    Window t (Window.MovedFromHunter eid) | t == timing ->
      member eid <$> select enemyMatcher
    _ -> pure False
  Matcher.PlaceUnderneath timing targetMatcher cardMatcher -> case window' of
    Window t (Window.PlaceUnderneath target' card) | t == timing -> liftA2
      (&&)
      (targetMatches target' targetMatcher)
      (pure $ cardMatch card cardMatcher)
    _ -> pure False
  Matcher.CommittedCard timing whoMatcher cardMatcher -> case window' of
    Window t (Window.CommittedCard who card) | t == timing ->
      liftA2 (&&) (matchWho who whoMatcher) (pure $ cardMatch card cardMatcher)
    _ -> pure False
  Matcher.CommittedCards timing whoMatcher cardListMatcher -> case window' of
    Window t (Window.CommittedCards who cards) | t == timing -> liftA2
      (&&)
      (matchWho who whoMatcher)
      (cardListMatches cards cardListMatcher)
    _ -> pure False
  Matcher.EnemyAttemptsToSpawnAt timing enemyMatcher locationMatcher ->
    case window' of
      Window t (Window.EnemyAttemptsToSpawnAt eid locationMatcher')
        | t == timing -> do
          case locationMatcher of
            Matcher.LocationNotInPlay -> do
              liftA2
                (&&)
                (enemyMatches eid enemyMatcher)
                (null <$> select locationMatcher')
            _ -> pure False -- TODO: We may need more things here
      _ -> pure False
  Matcher.TookControlOfAsset timing whoMatcher assetMatcher -> case window' of
    Window t (Window.TookControlOfAsset who aid) | t == timing ->
      liftA2 (&&) (matchWho who whoMatcher) (member aid <$> select assetMatcher)
    _ -> pure False
  Matcher.WouldDrawEncounterCard timing whoMatcher phaseMatcher ->
    case window' of
      Window t (Window.WouldDrawEncounterCard who p) | t == timing ->
        liftA2 (&&) (matchWho who whoMatcher) (matchPhase p phaseMatcher)
      _ -> pure False
  Matcher.AmongSearchedCards whoMatcher -> case window' of
    Window _ (Window.AmongSearchedCards who) -> matchWho who whoMatcher
    _ -> pure False
  Matcher.Discarded timing whoMatcher cardMatcher -> case window' of
    Window t (Window.Discarded who card) | t == timing ->
      (cardMatch card cardMatcher &&) <$> matchWho who whoMatcher
    _ -> pure False
  Matcher.AssetWouldBeDiscarded timing assetMatcher -> case window' of
    Window t (Window.WouldBeDiscarded (AssetTarget aid)) | t == timing ->
      elem aid <$> select assetMatcher
    _ -> pure False
  Matcher.EnemyWouldBeDiscarded timing enemyMatcher -> case window' of
    Window t (Window.WouldBeDiscarded (EnemyTarget eid)) | t == timing ->
      elem eid <$> select enemyMatcher
    _ -> pure False
  Matcher.AgendaAdvances timingMatcher agendaMatcher -> case window' of
    Window t (Window.AgendaAdvance aid) | t == timingMatcher ->
      agendaMatches aid agendaMatcher
    _ -> pure False
  Matcher.MovedBy timingMatcher whoMatcher sourceMatcher -> case window' of
    Window t (Window.MovedBy source' _ who) | t == timingMatcher -> liftA2
      (&&)
      (matchWho who whoMatcher)
      (sourceMatches source' sourceMatcher)
    _ -> pure False
  Matcher.MovedButBeforeEnemyEngagement timingMatcher whoMatcher whereMatcher
    -> case window' of
      Window t (Window.MovedButBeforeEnemyEngagement who locationId)
        | t == timingMatcher -> liftA2
          (&&)
          (matchWho who whoMatcher)
          (locationMatches iid source window' locationId whereMatcher)
      _ -> pure False
  Matcher.InvestigatorDefeated timingMatcher sourceMatcher whoMatcher ->
    case window' of
      Window t (Window.InvestigatorDefeated source' who) | t == timingMatcher ->
        liftA2
          (&&)
          (matchWho who whoMatcher)
          (sourceMatches source' sourceMatcher)
      _ -> pure False
  Matcher.AgendaWouldAdvance timingMatcher advancementReason agendaMatcher ->
    case window' of
      Window t (Window.AgendaWouldAdvance advancementReason' aid)
        | t == timingMatcher && advancementReason == advancementReason' -> agendaMatches
          aid
          agendaMatcher
      _ -> pure False
  Matcher.PlacedCounter whenMatcher whoMatcher counterMatcher valueMatcher ->
    case window' of
      Window t (Window.PlacedHorror iid' n)
        | t == whenMatcher && counterMatcher == Matcher.HorrorCounter -> liftA2
          (&&)
          (matchWho iid' whoMatcher)
          (gameValueMatches n valueMatcher)
      Window t (Window.PlacedDamage iid' n)
        | t == whenMatcher && counterMatcher == Matcher.DamageCounter -> liftA2
          (&&)
          (matchWho iid' whoMatcher)
          (gameValueMatches n valueMatcher)
      _ -> pure False
  Matcher.PlacedCounterOnLocation whenMatcher whereMatcher counterMatcher valueMatcher
    -> case window' of
      Window t (Window.PlacedClues (LocationTarget locationId) n)
        | t == whenMatcher && counterMatcher == Matcher.ClueCounter -> liftA2
          (&&)
          (locationMatches iid source window' locationId whereMatcher)
          (gameValueMatches n valueMatcher)
      _ -> pure False
  Matcher.PlacedCounterOnEnemy whenMatcher enemyMatcher counterMatcher valueMatcher
    -> case window' of
      Window t (Window.PlacedClues (EnemyTarget enemyId) n)
        | t == whenMatcher && counterMatcher == Matcher.ClueCounter -> liftA2
          (&&)
          (enemyMatches enemyId enemyMatcher)
          (gameValueMatches n valueMatcher)
      _ -> pure False
  Matcher.RevealLocation timingMatcher whoMatcher locationMatcher ->
    case window' of
      Window t (Window.RevealLocation who locationId) | t == timingMatcher ->
        liftA2
          (&&)
          (matchWho who whoMatcher)
          (locationMatches iid source window' locationId locationMatcher)
      _ -> pure False
  Matcher.GameEnds timingMatcher -> case window' of
    Window t Window.EndOfGame -> pure $ t == timingMatcher
    _ -> pure False
  Matcher.InvestigatorEliminated timingMatcher whoMatcher -> case window' of
    Window t (Window.InvestigatorEliminated who) | t == timingMatcher ->
      matchWho who whoMatcher
    _ -> pure False
  Matcher.PutLocationIntoPlay timingMatcher whoMatcher locationMatcher ->
    case window' of
      Window t (Window.PutLocationIntoPlay who locationId)
        | t == timingMatcher -> liftA2
          (&&)
          (matchWho who whoMatcher)
          (locationMatches iid source window' locationId locationMatcher)
      _ -> pure False
  Matcher.PlayerHasPlayableCard cardMatcher -> do
    -- TODO: do we need to grab the card source?
    -- cards <- filter (/= c) <$> getList cardMatcher
    cards <- withDepthLock 2 [] $ selectList cardMatcher
    anyM (getIsPlayable iid source UnpaidCost [window']) cards
  Matcher.PhaseBegins whenMatcher phaseMatcher -> case window' of
    Window t Window.AnyPhaseBegins | whenMatcher == t ->
      pure $ phaseMatcher == Matcher.AnyPhase
    Window t (Window.PhaseBegins p) | whenMatcher == t ->
      matchPhase p phaseMatcher
    _ -> pure False
  Matcher.PhaseEnds whenMatcher phaseMatcher -> case window' of
    Window t (Window.PhaseEnds p) | whenMatcher == t ->
      matchPhase p phaseMatcher
    _ -> pure False
  Matcher.TurnBegins whenMatcher whoMatcher -> case window' of
    Window t (Window.TurnBegins who) | t == whenMatcher ->
      matchWho who whoMatcher
    _ -> pure False
  Matcher.TurnEnds whenMatcher whoMatcher -> case window' of
    Window t (Window.TurnEnds who) | t == whenMatcher -> matchWho who whoMatcher
    _ -> pure False
  Matcher.RoundEnds whenMatcher -> case window' of
    Window t Window.AtEndOfRound -> pure $ t == whenMatcher
    _ -> pure False
  Matcher.Enters whenMatcher whoMatcher whereMatcher -> case window' of
    Window t (Window.Entering iid' lid) | whenMatcher == t -> liftA2
      (&&)
      (matchWho iid' whoMatcher)
      (locationMatches iid source window' lid whereMatcher)
    _ -> pure False
  Matcher.Leaves whenMatcher whoMatcher whereMatcher -> case window' of
    Window t (Window.Leaving iid' lid) | whenMatcher == t -> liftA2
      (&&)
      (matchWho iid' whoMatcher)
      (locationMatches iid source window' lid whereMatcher)
    _ -> pure False
  Matcher.Moves whenMatcher whoMatcher fromMatcher toMatcher -> case window' of
    Window t (Window.Moves iid' fromLid toLid) | whenMatcher == t -> andM
      [ matchWho iid' whoMatcher
      , locationMatches iid source window' fromLid fromMatcher
      , locationMatches iid source window' toLid toMatcher
      ]
    _ -> pure False
  Matcher.MoveAction whenMatcher whoMatcher fromMatcher toMatcher ->
    case window' of
      Window t (Window.MoveAction iid' fromLid toLid) | whenMatcher == t -> andM
        [ matchWho iid' whoMatcher
        , locationMatches iid source window' fromLid fromMatcher
        , locationMatches iid source window' toLid toMatcher
        ]
      _ -> pure False
  Matcher.PerformAction whenMatcher whoMatcher actionMatcher -> case window' of
    Window t (Window.PerformAction iid' action) | whenMatcher == t ->
      andM [matchWho iid' whoMatcher, actionMatches action actionMatcher]
    _ -> pure False
  Matcher.WouldHaveSkillTestResult whenMatcher whoMatcher _ skillTestResultMatcher
    -> case skillTestResultMatcher of
      Matcher.FailureResult _ -> case window' of
        Window t (Window.WouldFailSkillTest who) | t == whenMatcher ->
          matchWho who whoMatcher
        _ -> pure False
      Matcher.SuccessResult _ -> case window' of
        Window t (Window.WouldPassSkillTest who) | t == whenMatcher ->
          matchWho who whoMatcher
        _ -> pure False
      Matcher.AnyResult -> case window' of
        Window Timing.When (Window.WouldFailSkillTest who) ->
          matchWho who whoMatcher
        Window Timing.When (Window.WouldPassSkillTest who) ->
          matchWho who whoMatcher
        _ -> pure False
  Matcher.InitiatedSkillTest whenMatcher whoMatcher _ valueMatcher ->
    case window' of
      Window t (Window.InitiatedSkillTest who _ difficulty)
        | t == whenMatcher -> liftA2
          (&&)
          (matchWho who whoMatcher)
          (gameValueMatches difficulty valueMatcher)
      _ -> pure False
  Matcher.SkillTestEnded whenMatcher whoMatcher skillTestMatcher ->
    case window' of
      Window t (Window.SkillTestEnded skillTest) | whenMatcher == t -> liftA2
        (&&)
        (matchWho (skillTestInvestigator skillTest) whoMatcher)
        (skillTestMatches iid source skillTest skillTestMatcher)
      _ -> pure False
  Matcher.SkillTestResult whenMatcher whoMatcher skillMatcher skillTestResultMatcher
    -> do
      mskillTest <- getSkillTest
      matchSkillTest <- case mskillTest of
        Nothing -> pure False
        Just st -> skillTestMatches iid source st skillMatcher
      if not matchSkillTest
        then pure False
        else case skillTestResultMatcher of
          Matcher.FailureResult gameValueMatcher -> case window' of
            Window t (Window.FailInvestigationSkillTest who lid n)
              | whenMatcher == t -> case skillMatcher of
                Matcher.WhileInvestigating whereMatcher -> andM
                  [ matchWho who whoMatcher
                  , gameValueMatches n gameValueMatcher
                  , locationMatches iid source window' lid whereMatcher
                  ]
                _ -> pure False
            Window t (Window.FailAttackEnemy who enemyId n)
              | whenMatcher == t -> case skillMatcher of
                Matcher.WhileAttackingAnEnemy enemyMatcher -> andM
                  [ matchWho who whoMatcher
                  , gameValueMatches n gameValueMatcher
                  , enemyMatches enemyId enemyMatcher
                  ]
                _ -> pure False
            Window t (Window.FailEvadeEnemy who enemyId n) | whenMatcher == t ->
              case skillMatcher of
                Matcher.WhileEvadingAnEnemy enemyMatcher -> andM
                  [ matchWho who whoMatcher
                  , gameValueMatches n gameValueMatcher
                  , enemyMatches enemyId enemyMatcher
                  ]
                _ -> pure False
            Window t (Window.FailSkillTest who n) | whenMatcher == t -> andM
              [matchWho who whoMatcher, gameValueMatches n gameValueMatcher]
            _ -> pure False
          Matcher.SuccessResult gameValueMatcher -> case window' of
            Window t (Window.PassInvestigationSkillTest who lid n)
              | whenMatcher == t -> case skillMatcher of
                Matcher.WhileInvestigating whereMatcher -> andM
                  [ matchWho who whoMatcher
                  , gameValueMatches n gameValueMatcher
                  , locationMatches iid source window' lid whereMatcher
                  ]
                _ -> pure False
            Window t (Window.SuccessfulAttackEnemy who enemyId n)
              | whenMatcher == t -> case skillMatcher of
                Matcher.WhileAttackingAnEnemy enemyMatcher -> andM
                  [ matchWho who whoMatcher
                  , gameValueMatches n gameValueMatcher
                  , enemyMatches enemyId enemyMatcher
                  ]
                _ -> pure False
            Window t (Window.SuccessfulEvadeEnemy who enemyId n)
              | whenMatcher == t -> case skillMatcher of
                Matcher.WhileEvadingAnEnemy enemyMatcher -> andM
                  [ matchWho who whoMatcher
                  , gameValueMatches n gameValueMatcher
                  , enemyMatches enemyId enemyMatcher
                  ]
                _ -> pure False
            Window t (Window.PassSkillTest _ _ who n)
              | whenMatcher == t && skillMatcher == Matcher.AnySkillTest -> liftA2
                (&&)
                (matchWho who whoMatcher)
                (gameValueMatches n gameValueMatcher)
            _ -> pure False
          Matcher.AnyResult -> case window' of
            Window t (Window.FailSkillTest who _) | whenMatcher == t ->
              matchWho who whoMatcher
            Window t (Window.PassSkillTest _ _ who _) | whenMatcher == t ->
              matchWho who whoMatcher
            _ -> pure False
  Matcher.DuringTurn whoMatcher -> case window' of
    Window Timing.When Window.NonFast -> matchWho iid whoMatcher
    Window Timing.When (Window.DuringTurn who) -> matchWho who whoMatcher
    Window Timing.When Window.FastPlayerWindow -> do
      miid <- selectOne Matcher.TurnInvestigator
      pure $ Just iid == miid
    _ -> pure False
  Matcher.OrWindowMatcher matchers ->
    anyM (windowMatches iid source window') matchers
  Matcher.EnemySpawns timingMatcher whereMatcher enemyMatcher ->
    case window' of
      Window t (Window.EnemySpawns enemyId locationId) | t == timingMatcher ->
        liftA2
          (&&)
          (enemyMatches enemyId enemyMatcher)
          (locationMatches iid source window' locationId whereMatcher)
      _ -> pure False
  Matcher.EnemyWouldAttack timingMatcher whoMatcher enemyAttackMatcher enemyMatcher
    -> case window' of
      Window t (Window.EnemyWouldAttack who enemyId enemyAttackType)
        | timingMatcher == t -> andM
          [ matchWho who whoMatcher
          , enemyMatches enemyId enemyMatcher
          , pure $ enemyAttackMatches enemyAttackType enemyAttackMatcher
          ]
      _ -> pure False
  Matcher.EnemyAttacks timingMatcher whoMatcher enemyAttackMatcher enemyMatcher
    -> case window' of
      Window t (Window.EnemyAttacks who enemyId enemyAttackType)
        | timingMatcher == t -> andM
          [ matchWho who whoMatcher
          , enemyMatches enemyId enemyMatcher
          , pure $ enemyAttackMatches enemyAttackType enemyAttackMatcher
          ]
      _ -> pure False
  Matcher.EnemyAttacked timingMatcher whoMatcher sourceMatcher enemyMatcher ->
    case window' of
      Window t (Window.EnemyAttacked who attackSource enemyId)
        | timingMatcher == t -> andM
          [ matchWho who whoMatcher
          , enemyMatches enemyId enemyMatcher
          , sourceMatches attackSource sourceMatcher
          ]
      _ -> pure False
  Matcher.EnemyEvaded timingMatcher whoMatcher enemyMatcher -> case window' of
    Window t (Window.EnemyEvaded who enemyId) | timingMatcher == t ->
      liftA2 (&&) (enemyMatches enemyId enemyMatcher) (matchWho who whoMatcher)
    _ -> pure False
  Matcher.EnemyEngaged timingMatcher whoMatcher enemyMatcher -> case window' of
    Window t (Window.EnemyEngaged who enemyId) | timingMatcher == t ->
      liftA2 (&&) (enemyMatches enemyId enemyMatcher) (matchWho who whoMatcher)
    _ -> pure False
  Matcher.MythosStep mythosStepMatcher -> case window' of
    Window t Window.AllDrawEncounterCard | t == Timing.When ->
      pure $ mythosStepMatcher == Matcher.WhenAllDrawEncounterCard
    _ -> pure False
  Matcher.WouldRevealChaosToken whenMatcher whoMatcher -> case window' of
    Window t (Window.WouldRevealChaosToken _ who) | whenMatcher == t ->
      matchWho who whoMatcher
    _ -> pure False
  Matcher.RevealChaosToken whenMatcher whoMatcher tokenMatcher ->
    case window' of
      Window t (Window.RevealToken who token) | whenMatcher == t -> liftA2
        (&&)
        (matchWho who whoMatcher)
        (matchToken who token tokenMatcher)
      _ -> pure False
  Matcher.AddedToVictory timingMatcher cardMatcher -> case window' of
    Window t (Window.AddedToVictory card) | timingMatcher == t ->
      pure $ cardMatch card cardMatcher
    _ -> pure False
  Matcher.AssetDefeated timingMatcher assetMatcher -> case window' of
    Window t (Window.AssetDefeated assetId) | timingMatcher == t ->
      member assetId <$> select assetMatcher
    _ -> pure False
  Matcher.EnemyDefeated timingMatcher whoMatcher enemyMatcher ->
    case window' of
      Window t (Window.EnemyDefeated who enemyId) | timingMatcher == t -> liftA2
        (&&)
        (enemyMatches enemyId enemyMatcher)
        (matchWho who whoMatcher)
      _ -> pure False
  Matcher.EnemyEnters timingMatcher whereMatcher enemyMatcher ->
    case window' of
      Window t (Window.EnemyEnters enemyId lid) | timingMatcher == t -> liftA2
        (&&)
        (enemyMatches enemyId enemyMatcher)
        (locationMatches iid source window' lid whereMatcher)
      _ -> pure False
  Matcher.EnemyLeaves timingMatcher whereMatcher enemyMatcher ->
    case window' of
      Window t (Window.EnemyLeaves enemyId lid) | timingMatcher == t -> liftA2
        (&&)
        (enemyMatches enemyId enemyMatcher)
        (locationMatches iid source window' lid whereMatcher)
      _ -> pure False
  Matcher.ChosenRandomLocation timingMatcher whereMatcher -> case window' of
    Window t (Window.ChosenRandomLocation lid) | timingMatcher == t ->
      locationMatches iid source window' lid whereMatcher
    _ -> pure False
  Matcher.EnemyWouldBeDefeated timingMatcher enemyMatcher -> case window' of
    Window t (Window.EnemyWouldBeDefeated enemyId) | timingMatcher == t ->
      enemyMatches enemyId enemyMatcher
    _ -> pure False
  Matcher.EnemyWouldReady timingMatcher enemyMatcher -> case window' of
    Window t (Window.WouldReady (EnemyTarget enemyId)) | timingMatcher == t ->
      enemyMatches enemyId enemyMatcher
    _ -> pure False
  Matcher.FastPlayerWindow -> case window' of
    Window Timing.When Window.FastPlayerWindow -> pure True
    _ -> pure False
  Matcher.DealtDamageOrHorror whenMatcher whoMatcher -> case whoMatcher of
    Matcher.You -> case window' of
      Window t (Window.WouldTakeDamageOrHorror _ (InvestigatorTarget iid') _ _)
        | t == whenMatcher -> pure $ iid == iid'
      _ -> pure False
    _ -> pure False
  Matcher.DealtDamage whenMatcher whoMatcher -> case window' of
    Window t (Window.DealtDamage _ _ (InvestigatorTarget iid'))
      | t == whenMatcher -> matchWho iid' whoMatcher
    _ -> pure False
  Matcher.DealtHorror whenMatcher whoMatcher -> case window' of
    Window t (Window.DealtHorror _ (InvestigatorTarget iid'))
      | t == whenMatcher -> matchWho iid' whoMatcher
    _ -> pure False
  Matcher.AssignedHorror whenMatcher whoMatcher targetListMatcher ->
    case window' of
      Window t (Window.AssignedHorror _ who targets) | t == whenMatcher ->
        liftA2
          (&&)
          (matchWho who whoMatcher)
          (targetListMatches targets targetListMatcher)
      _ -> pure False
  Matcher.AssetDealtDamage timingMatcher assetMatcher -> case window' of
    Window t (Window.DealtDamage _ _ (AssetTarget aid)) | t == timingMatcher ->
      member aid <$> select assetMatcher
    _ -> pure False
  Matcher.EnemyDealtDamage timingMatcher damageEffectMatcher enemyMatcher sourceMatcher
    -> case window' of
      Window t (Window.DealtDamage source' damageEffect (EnemyTarget eid))
        | t == timingMatcher -> andM
          [ damageEffectMatches damageEffect damageEffectMatcher
          , member eid <$> select enemyMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> pure False
  Matcher.EnemyTakeDamage timingMatcher damageEffectMatcher enemyMatcher sourceMatcher
    -> case window' of
      Window t (Window.TakeDamage source' damageEffect (EnemyTarget eid))
        | t == timingMatcher -> andM
          [ damageEffectMatches damageEffect damageEffectMatcher
          , member eid <$> select enemyMatcher
          , sourceMatches source' sourceMatcher
          ]
      _ -> pure False
  Matcher.DiscoverClues whenMatcher whoMatcher whereMatcher valueMatcher ->
    case window' of
      Window t (Window.DiscoverClues who lid n) | whenMatcher == t -> andM
        [ matchWho who whoMatcher
        , locationMatches iid source window' lid whereMatcher
        , gameValueMatches n valueMatcher
        ]
      _ -> pure False
  Matcher.GainsClues whenMatcher whoMatcher valueMatcher -> case window' of
    Window t (Window.GainsClues who n) | whenMatcher == t ->
      andM [matchWho who whoMatcher, gameValueMatches n valueMatcher]
    _ -> pure False
  Matcher.DiscoveringLastClue whenMatcher whoMatcher whereMatcher ->
    case window' of
      Window t (Window.DiscoveringLastClue who lid) | whenMatcher == t -> liftA2
        (&&)
        (matchWho who whoMatcher)
        (locationMatches iid source window' lid whereMatcher)
      _ -> pure False
  Matcher.LastClueRemovedFromAsset whenMatcher assetMatcher -> case window' of
    Window t (Window.LastClueRemovedFromAsset aid) | whenMatcher == t ->
      member aid <$> select assetMatcher
    _ -> pure False
  Matcher.DrawCard whenMatcher whoMatcher cardMatcher deckMatcher ->
    case window' of
      Window t (Window.DrawCard who card deck) | whenMatcher == t -> andM
        [ matchWho who whoMatcher
        , case cardMatcher of
          Matcher.BasicCardMatch baseMatcher ->
            pure $ cardMatch card baseMatcher
          _ -> member card <$> select cardMatcher
        , deckMatch iid deck deckMatcher
        ]
      _ -> pure False
  Matcher.DeckHasNoCards whenMatcher whoMatcher -> case window' of
    Window t (Window.DeckHasNoCards who) | whenMatcher == t ->
      matchWho who whoMatcher
    _ -> pure False
  Matcher.PlayCard whenMatcher whoMatcher cardMatcher -> case window' of
    Window t (Window.PlayCard who card) | whenMatcher == t ->
      liftA2 (&&) (matchWho who whoMatcher) (member card <$> select cardMatcher)
    _ -> pure False
  Matcher.AssetEntersPlay timingMatcher assetMatcher -> case window' of
    Window t (Window.EnterPlay (AssetTarget aid)) | t == timingMatcher ->
      member aid <$> select assetMatcher
    _ -> pure False
  Matcher.AssetLeavesPlay timingMatcher assetMatcher -> case window' of
    Window t (Window.LeavePlay (AssetTarget aid)) | t == timingMatcher ->
      member aid <$> select assetMatcher
    _ -> pure False
  Matcher.LocationLeavesPlay timingMatcher locationMatcher -> case window' of
    Window t (Window.LeavePlay (LocationTarget aid)) | t == timingMatcher ->
      member aid <$> select locationMatcher
    _ -> pure False
  Matcher.EnemyLeavesPlay timingMatcher enemyMatcher -> case window' of
    Window t (Window.LeavePlay (EnemyTarget eid)) | t == timingMatcher ->
      member eid <$> select enemyMatcher
    _ -> pure False

matchWho
  :: (Monad m, Query Matcher.InvestigatorMatcher m)
  => InvestigatorId
  -> Matcher.InvestigatorMatcher
  -> m Bool
matchWho who matcher = member who <$> select matcher

gameValueMatches
  :: (Query Matcher.InvestigatorMatcher m, Monad m)
  => Int
  -> Matcher.ValueMatcher
  -> m Bool
gameValueMatches n = \case
  Matcher.AnyValue -> pure True
  Matcher.LessThan gv -> (n <) <$> getPlayerCountValue gv
  Matcher.GreaterThan gv -> (n >) <$> getPlayerCountValue gv
  Matcher.LessThanOrEqualTo gv -> (n <=) <$> getPlayerCountValue gv
  Matcher.GreaterThanOrEqualTo gv -> (n >=) <$> getPlayerCountValue gv
  Matcher.EqualTo gv -> (n ==) <$> getPlayerCountValue gv

sourceTraits
  :: ( Projection m AssetAttrs
     , Projection m EnemyAttrs
     , Projection m EventAttrs
     , Projection m InvestigatorAttrs
     , Projection m LocationAttrs
     , Projection m SkillAttrs
     , Projection m TreacheryAttrs
     , Monad m
     )
  => Source
  -> m (HashSet Trait)
sourceTraits = \case
  AbilitySource s _ -> sourceTraits s
  ActDeckSource -> pure mempty
  ActSource _ -> pure mempty
  AfterSkillTestSource -> pure mempty
  AgendaDeckSource -> pure mempty
  AgendaSource _ -> pure mempty
  AssetMatcherSource _ -> pure mempty
  AssetSource aid -> field AssetTraits aid

  CardCodeSource _ -> pure mempty
  CardIdSource _ -> pure mempty
  DeckSource -> pure mempty
  EffectSource _ -> pure mempty
  EmptyDeckSource -> pure mempty
  EncounterCardSource _ -> pure mempty
  EnemyAttackSource _ -> pure mempty

  EnemySource eid -> field EnemyTraits eid
  EventSource eid -> field EventTraits eid

  GameSource -> pure mempty

  InvestigatorSource iid -> field InvestigatorTraits iid
  LocationSource lid -> field LocationTraits lid

  PlayerCardSource _ -> pure mempty
  ProxySource s _ -> sourceTraits s
  ResourceSource -> pure mempty

  ScenarioSource _ -> pure mempty
  SkillSource sid -> field SkillTraits sid
  SkillTestSource{} -> pure mempty
  TreacherySource tid -> field TreacheryTraits tid

  StorySource _ -> pure mempty
  TestSource traits -> pure traits
  TokenEffectSource _ -> pure mempty
  TokenSource _ -> pure mempty

sourceMatches
  :: ( Projection m AssetAttrs
     , Projection m EnemyAttrs
     , Projection m EventAttrs
     , Projection m InvestigatorAttrs
     , Projection m LocationAttrs
     , Projection m SkillAttrs
     , Projection m TreacheryAttrs
     , Query Matcher.InvestigatorMatcher m
     , Query Matcher.EnemyMatcher m
     )
  => Source
  -> Matcher.SourceMatcher
  -> m Bool
sourceMatches s = \case
  Matcher.SourceMatchesAny ms -> anyM (sourceMatches s) ms
  Matcher.SourceWithTrait t -> elem t <$> sourceTraits s
  Matcher.SourceIsEnemyAttack em -> case s of
    EnemyAttackSource eid -> member eid <$> select em
    _ -> pure False
  Matcher.AnySource -> pure True
  Matcher.SourceMatches ms -> allM (sourceMatches s) ms
  Matcher.SourceOwnedBy whoMatcher -> case s of
    AssetSource aid -> do
      mControllerId <- selectAssetController aid
      case mControllerId of
        Just iid' -> member iid' <$> select whoMatcher
        _ -> pure False
    EventSource eid -> do
      mControllerId <- fromJustNote "must have a controller"
        <$> selectEventController eid
      member mControllerId <$> select whoMatcher
    SkillSource sid -> do
      mControllerId <- fromJustNote "must have a controller"
        <$> selectSkillController sid
      member mControllerId <$> select whoMatcher
    _ -> pure False
  Matcher.SourceIsType t -> pure $ case t of
    AssetType -> case s of
      AssetSource _ -> True
      _ -> False
    EventType -> case s of
      EventSource _ -> True
      _ -> False
    SkillType -> case s of
      SkillSource _ -> True
      _ -> False
    PlayerTreacheryType -> case s of
      TreacherySource _ -> True
      _ -> False
    PlayerEnemyType -> case s of
      EnemySource _ -> True
      _ -> False
    TreacheryType -> case s of
      TreacherySource _ -> True
      _ -> False
    EnemyType -> case s of
      EnemySource _ -> True
      _ -> False
    LocationType -> case s of
      LocationSource _ -> True
      _ -> False
    EncounterAssetType -> case s of
      AssetSource _ -> True
      _ -> False
    ActType -> case s of
      ActSource _ -> True
      _ -> False
    AgendaType -> case s of
      AgendaSource _ -> True
      _ -> False
    StoryType -> case s of
      StorySource _ -> True
      _ -> False
    InvestigatorType -> case s of
      InvestigatorSource _ -> True
      _ -> False
  Matcher.EncounterCardSource -> pure $ case s of
    ActSource _ -> True
    AgendaSource _ -> True
    EnemySource _ -> True
    LocationSource _ -> True
    TreacherySource _ -> True
    _ -> False

targetMatches :: Monad m => Target -> Matcher.TargetMatcher -> m Bool
targetMatches s = \case
  Matcher.TargetMatchesAny ms -> anyM (targetMatches s) ms
  Matcher.TargetIs s' -> pure $ s == s'
  Matcher.AnyTarget -> pure True
  Matcher.TargetMatches ms -> allM (targetMatches s) ms

enemyMatches
  :: (Query Matcher.EnemyMatcher m) => EnemyId -> Matcher.EnemyMatcher -> m Bool
enemyMatches !enemyId !mtchr = member enemyId <$> select mtchr

locationMatches
  :: ( HasCallStack
     , Query Matcher.TreacheryMatcher m
     , Query Matcher.InvestigatorMatcher m
     , Query Matcher.LocationMatcher m
     , Query Matcher.EnemyMatcher m
     , Query Matcher.AssetMatcher m
     , Projection m InvestigatorAttrs
     , Projection m LocationAttrs
     , HasModifiersFor ()
     , Monad m
     )
  => InvestigatorId
  -> Source
  -> Window
  -> LocationId
  -> Matcher.LocationMatcher
  -> m Bool
locationMatches investigatorId source window locationId matcher =
  case matcher of
    Matcher.LocationNotInPlay -> pure False
    Matcher.LocationWithLabel label ->
      member locationId <$> select (Matcher.LocationWithLabel label)
    Matcher.LocationWithTitle title ->
      member locationId <$> select (Matcher.LocationWithTitle title)
    Matcher.LocationWithFullTitle title subtitle -> member locationId
      <$> select (Matcher.LocationWithFullTitle title subtitle)
    Matcher.LocationWithSymbol locationSymbol ->
      member locationId <$> select (Matcher.LocationWithSymbol locationSymbol)
    Matcher.LocationWithUnrevealedTitle title ->
      member locationId <$> select (Matcher.LocationWithUnrevealedTitle title)
    Matcher.LocationWithId lid -> pure $ lid == locationId
    Matcher.LocationIs cardCode ->
      member locationId <$> select (Matcher.LocationIs cardCode)
    Matcher.Anywhere -> pure True
    Matcher.Unblocked -> notElem Blocked <$> getModifiers
      (InvestigatorSource investigatorId)
      (LocationTarget locationId)
    Matcher.EmptyLocation -> member locationId <$> select matcher
    Matcher.LocationWithoutInvestigators ->
      member locationId <$> select matcher
    Matcher.LocationWithoutEnemies -> member locationId <$> select matcher
    Matcher.LocationWithEnemy enemyMatcher -> notNull <$> select
      (Matcher.EnemyAt (Matcher.LocationWithId locationId) <> enemyMatcher)
    Matcher.LocationWithAsset assetMatcher -> notNull <$> select
      (Matcher.AssetAt (Matcher.LocationWithId locationId) <> assetMatcher)
    Matcher.LocationWithInvestigator whoMatcher -> notNull <$> select
      (Matcher.InvestigatorAt (Matcher.LocationWithId locationId) <> whoMatcher)
    Matcher.AccessibleLocation ->
      member locationId <$> select Matcher.AccessibleLocation
    Matcher.AccessibleFrom _ -> member locationId <$> select matcher
    Matcher.AccessibleTo _ -> member locationId <$> select matcher
    Matcher.ConnectedLocation -> member locationId <$> select matcher
    Matcher.RevealedLocation -> member locationId <$> select matcher
    Matcher.UnrevealedLocation -> member locationId <$> select matcher
    Matcher.LocationWithClues valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationClues locationId
    Matcher.LocationWithDoom valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationDoom locationId
    Matcher.LocationWithHorror valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationHorror locationId
    Matcher.LocationWithMostClues locationMatcher -> member locationId
      <$> select (Matcher.LocationWithMostClues locationMatcher)
    Matcher.LocationWithResources valueMatcher ->
      (`gameValueMatches` valueMatcher) =<< field LocationResources locationId
    Matcher.LocationLeavingPlay -> case window of
      Window _ (Window.LeavePlay (LocationTarget lid)) ->
        pure $ locationId == lid
      _ -> error "invalid window for LocationLeavingPlay"
    Matcher.SameLocation -> do
      mlid' <- case source of
        EnemySource eid ->
          selectOne $ Matcher.LocationWithEnemy $ Matcher.EnemyWithId eid
        AssetSource aid ->
          selectOne $ Matcher.LocationWithAsset $ Matcher.AssetWithId aid
        _ -> error $ "can't detect same location for source " <> show source
      pure $ Just locationId == mlid'
    Matcher.YourLocation -> do
      yourLocationId <- field InvestigatorLocation investigatorId
      pure $ Just locationId == yourLocationId
    Matcher.ThisLocation -> case source of
      (LocationSource lid) -> pure $ lid == locationId
      (ProxySource (LocationSource lid) _) -> pure $ lid == locationId
      _ -> error "Invalid source for ThisLocation"
    Matcher.NotYourLocation -> do
      yourLocationId <- field InvestigatorLocation investigatorId
      pure $ Just locationId /= yourLocationId
    Matcher.LocationInDirection direction matcher' -> do
      member locationId
        <$> select (Matcher.LocationInDirection direction matcher')
    Matcher.FarthestLocationFromYou _ -> member locationId <$> select matcher
    Matcher.FarthestLocationFromLocation _ _ ->
      member locationId <$> select matcher
    Matcher.FarthestLocationFromAll _ -> do
      member locationId <$> select matcher
    Matcher.LocationWithDistanceFrom distance matcher' -> member locationId
      <$> select (Matcher.LocationWithDistanceFrom distance matcher')
    Matcher.NearestLocationToYou _ -> member locationId <$> select matcher
    Matcher.LocationWithTrait _ -> member locationId <$> select matcher
    Matcher.LocationWithoutTrait _ -> member locationId <$> select matcher
    Matcher.LocationMatchAll ms ->
      allM (locationMatches investigatorId source window locationId) ms
    Matcher.LocationMatchAny ms ->
      anyM (locationMatches investigatorId source window locationId) ms
    Matcher.FirstLocation ms ->
      anyM (locationMatches investigatorId source window locationId) ms -- a bit weird here since first means nothing
    Matcher.LocationWithoutTreachery treacheryMatcher -> do
      null <$> select
        (Matcher.TreacheryAt (Matcher.LocationWithId locationId)
        <> treacheryMatcher
        )
    Matcher.LocationWithTreachery treacheryMatcher -> do
      notNull <$> select
        (Matcher.TreacheryAt (Matcher.LocationWithId locationId)
        <> treacheryMatcher
        )
    Matcher.InvestigatableLocation -> do
      modifiers <- getModifiers
        (InvestigatorSource investigatorId)
        (LocationTarget locationId)
      pure $ CannotInvestigate `notElem` modifiers

skillTestMatches
  :: ( Query Matcher.SkillMatcher m
     , Query Matcher.EnemyMatcher m
     , Query Matcher.LocationMatcher m
     , Query Matcher.TreacheryMatcher m
     , Query Matcher.InvestigatorMatcher m
     , Projection m AssetAttrs
     , Projection m EnemyAttrs
     , Projection m EventAttrs
     , Projection m InvestigatorAttrs
     , Projection m LocationAttrs
     , Projection m SkillAttrs
     , Projection m TreacheryAttrs
     )
  => InvestigatorId
  -> Source
  -> SkillTest
  -> Matcher.SkillTestMatcher
  -> m Bool
skillTestMatches iid source st = \case
  Matcher.AnySkillTest -> pure True
  Matcher.SkillTestWasFailed -> pure $ case skillTestResult st of
    FailedBy _ _ -> True
    _ -> False
  Matcher.YourSkillTest matcher -> liftA2
    (&&)
    (pure $ skillTestInvestigator st == iid)
    (skillTestMatches iid source st matcher)
  Matcher.UsingThis -> pure $ source == skillTestSource st
  Matcher.SkillTestSourceMatches sourceMatcher ->
    sourceMatches (skillTestSource st) sourceMatcher
  Matcher.WhileInvestigating locationMatcher -> case skillTestAction st of
    Just Action.Investigate -> case skillTestTarget st of
      LocationTarget lid -> member lid <$> select locationMatcher
      _ -> pure False
    _ -> pure False
  Matcher.SkillTestOnTreachery treacheryMatcher -> case skillTestSource st of
    TreacherySource tid -> member tid <$> select treacheryMatcher
    _ -> pure False
  Matcher.WhileAttackingAnEnemy enemyMatcher -> case skillTestAction st of
    Just Action.Fight -> case skillTestTarget st of
      EnemyTarget eid -> member eid <$> select enemyMatcher
      _ -> pure False
    _ -> pure False
  Matcher.WhileEvadingAnEnemy enemyMatcher -> case skillTestAction st of
    Just Action.Evade -> case skillTestTarget st of
      EnemyTarget eid -> member eid <$> select enemyMatcher
      _ -> pure False
    _ -> pure False
  Matcher.SkillTestWithSkill sk -> notNull <$> select sk
  Matcher.SkillTestWithSkillType sType -> pure $ skillTestSkillType st == sType
  Matcher.SkillTestAtYourLocation -> do
    mlid1 <- field InvestigatorLocation iid
    mlid2 <- field InvestigatorLocation $ skillTestInvestigator st
    case (mlid1, mlid2) of
      (Just lid1, Just lid2) -> pure $ lid1 == lid2
      _ -> pure False
  Matcher.SkillTestMatches ms -> allM (skillTestMatches iid source st) ms

matchToken
  :: (Monad m, HasTokenValue m (), Projection m LocationAttrs)
  => InvestigatorId
  -> Token
  -> Matcher.TokenMatcher
  -> m Bool
matchToken iid' t = \case
  Matcher.WithNegativeModifier -> do
    tv <- getTokenValue iid' (tokenFace t) ()
    case tv of
      TokenValue _ (NegativeModifier _) -> pure True
      TokenValue _ (DoubleNegativeModifier _) -> pure True
      _ -> pure False
  Matcher.TokenFaceIs face -> pure $ face == tokenFace t
  Matcher.TokenFaceIsNot face -> pure $ face /= tokenFace t
  Matcher.AnyToken -> pure True
  Matcher.TokenMatchesAny ms -> anyM (matchToken iid' t) ms
  Matcher.TokenMatches ms -> allM (matchToken iid' t) ms

matchPhase :: Monad m => Phase -> Matcher.PhaseMatcher -> m Bool
matchPhase p = \case
  Matcher.AnyPhase -> pure True
  Matcher.PhaseIs p' -> pure $ p == p'

getModifiedTokenFaces
  :: (SourceEntity source, HasModifiersFor (), Monad m)
  => source
  -> [Token]
  -> m [TokenFace]
getModifiedTokenFaces source tokens = flip
  concatMapM
  tokens
  \token -> do
    modifiers' <- getModifiers (toSource source) (TokenTarget token)
    pure $ foldl' applyModifier [tokenFace token] modifiers'
 where
  applyModifier _ (TokenFaceModifier fs') = fs'
  applyModifier [f'] (ForcedTokenChange f fs) | f == f' = fs
  applyModifier fs _ = fs

cardListMatches
  :: (Monad m, Query Matcher.InvestigatorMatcher m)
  => [Card]
  -> Matcher.CardListMatcher
  -> m Bool
cardListMatches cards = \case
  Matcher.AnyCards -> pure True
  Matcher.LengthIs valueMatcher -> gameValueMatches (length cards) valueMatcher
  Matcher.HasCard cardMatcher -> pure $ any (`cardMatch` cardMatcher) cards

targetListMatches :: Monad m => [Target] -> Matcher.TargetListMatcher -> m Bool
targetListMatches targets = \case
  Matcher.AnyTargetList -> pure True
  Matcher.HasTarget targetMatcher ->
    anyM (`targetMatches` targetMatcher) targets
  Matcher.ExcludesTarget targetMatcher ->
    noneM (`targetMatches` targetMatcher) targets

rememberedListMatches
  :: (Monad m, Query Matcher.InvestigatorMatcher m)
  => [ScenarioLogKey]
  -> Matcher.ScenarioLogKeyListMatcher
  -> m Bool
rememberedListMatches targets = \case
  Matcher.HasRemembered k -> pure $ k `elem` targets
  Matcher.RememberedLengthIs valueMatcher ->
    gameValueMatches (length targets) valueMatcher

deckMatch
  :: (Monad m, Query Matcher.InvestigatorMatcher m)
  => InvestigatorId
  -> DeckSignifier
  -> Matcher.DeckMatcher
  -> m Bool
deckMatch iid deckSignifier = \case
  Matcher.EncounterDeck -> pure $ deckSignifier == EncounterDeck
  Matcher.DeckOf investigatorMatcher -> matchWho iid investigatorMatcher
  Matcher.AnyDeck -> pure True

agendaMatches
  :: Query Matcher.AgendaMatcher m
  => AgendaId
  -> Matcher.AgendaMatcher
  -> m Bool
agendaMatches !agendaId !mtchr = member agendaId <$> select mtchr

actionMatches :: Applicative m => Action -> Matcher.ActionMatcher -> m Bool
actionMatches a (Matcher.ActionIs a') = pure $ a == a'

enemyAttackMatches :: EnemyAttackType -> Matcher.EnemyAttackMatcher -> Bool
enemyAttackMatches atkType = \case
  Matcher.AnyEnemyAttack -> True
  Matcher.AttackOfOpportunityAttack -> atkType == AttackOfOpportunity

damageEffectMatches
  :: Applicative m => DamageEffect -> Matcher.DamageEffectMatcher -> m Bool
damageEffectMatches a = \case
  Matcher.AnyDamageEffect -> pure True
  Matcher.AttackDamageEffect -> pure $ a == AttackDamageEffect
  Matcher.NonAttackDamageEffect -> pure $ a == NonAttackDamageEffect

spawnAtOneOf
  :: ( MonadIO m
     , MonadReader env m
     , HasQueue env
     , Query Matcher.LocationMatcher m
     )
  => InvestigatorId
  -> EnemyId
  -> [LocationId]
  -> m ()
spawnAtOneOf iid eid targetLids = do
  locations' <- select Matcher.Anywhere
  case setToList (setFromList targetLids `intersection` locations') of
    [] -> push (Discard (EnemyTarget eid))
    [lid] -> pushAll (resolve $ EnemySpawn Nothing lid eid)
    lids -> push
      (chooseOne
        iid
        [ TargetLabel
            (LocationTarget lid)
            (resolve $ EnemySpawn Nothing lid eid)
        | lid <- lids
        ]
      )

sourceCanDamageEnemy
  :: ( HasModifiersFor ()
     , Monad m
     , Query Matcher.InvestigatorMatcher m
     , Query Matcher.EnemyMatcher m
     , Projection m AssetAttrs
     , Projection m EnemyAttrs
     , Projection m EventAttrs
     , Projection m InvestigatorAttrs
     , Projection m LocationAttrs
     , Projection m SkillAttrs
     , Projection m TreacheryAttrs
     )
  => EnemyId
  -> Source
  -> m Bool
sourceCanDamageEnemy eid source = do
  modifiers' <- getModifiers source (EnemyTarget eid)
  not <$> anyM prevents modifiers'
 where
  prevents = \case
    CannotBeDamagedByPlayerSourcesExcept matcher -> not <$> sourceMatches
      source
      (Matcher.SourceMatchesAny [Matcher.EncounterCardSource, matcher])
    CannotBeDamaged -> pure True
    _ -> pure False

getCanShuffleDeck :: (HasModifiersFor (), Monad m) => InvestigatorId -> m Bool
getCanShuffleDeck iid = do
  modifiers <- getModifiers (InvestigatorSource iid) (InvestigatorTarget iid)
  pure $ CannotManipulateDeck `notElem` modifiers

remembered
  :: (Functor m, Projection m ScenarioAttrs, Query Matcher.ScenarioMatcher m)
  => ScenarioLogKey
  -> m Bool
remembered k = member k <$> scenarioField ScenarioRemembered

getDoomCount
  :: ( Query Matcher.AssetMatcher m
     , Query Matcher.EnemyMatcher m
     , Query Matcher.LocationMatcher m
     , Query Matcher.TreacheryMatcher m
     , Query Matcher.AgendaMatcher m
     , Query Matcher.InvestigatorMatcher m
     , Projection m AssetAttrs
     , Projection m EnemyAttrs
     , Projection m LocationAttrs
     , Projection m TreacheryAttrs
     , Projection m AgendaAttrs
     , Projection m InvestigatorAttrs
     )
  => m Int
getDoomCount = sum <$> sequence
  [ selectAgg (+) AssetDoom Matcher.AnyAsset
  , selectAgg (+) EnemyDoom Matcher.AnyEnemy
  , selectAgg (+) LocationDoom Matcher.Anywhere
  , selectAgg (+) TreacheryDoom Matcher.AnyTreachery
  , selectAgg (+) AgendaDoom Matcher.AnyAgenda
  , selectAgg (+) InvestigatorDoom Matcher.Anyone
  ]

getPotentialSlots
  :: (Query Matcher.AssetMatcher m, Projection m InvestigatorAttrs)
  => HashSet Trait
  -> InvestigatorId
  -> m [SlotType]
getPotentialSlots traits iid = do
  slots <- field InvestigatorSlots iid
  let
    slotTypesAndSlots :: [(SlotType, Slot)] =
      concatMap (\(slotType, slots') -> map (slotType, ) slots')
        $ mapToList slots
    passesRestriction = \case
      TraitRestrictedSlot _ t _ -> t `member` traits
      Slot{} -> True
  map fst
    <$> filterM
          (\(_, slot) -> if passesRestriction slot
            then case slotItem slot of
              Nothing -> pure True
              Just aid -> member aid <$> select Matcher.DiscardableAsset
            else pure False
          )
          slotTypesAndSlots

type CanGetAbilities m
  = ( Projection m EnemyAttrs
    , Projection m LocationAttrs
    , Projection m AssetAttrs
    , Projection m TreacheryAttrs
    , Projection m ActAttrs
    , Projection m AgendaAttrs
    , Projection m EventAttrs
    , Projection m EffectAttrs
    , Projection m InvestigatorAttrs
    , Query Matcher.EnemyMatcher m
    , Query Matcher.LocationMatcher m
    , Query Matcher.AssetMatcher m
    , Query Matcher.TreacheryMatcher m
    , Query Matcher.ActMatcher m
    , Query Matcher.AgendaMatcher m
    , Query Matcher.EventMatcher m
    , Query Matcher.EffectMatcher m
    , Query Matcher.InvestigatorMatcher m
    )

getAllAbilities :: CanGetAbilities m => m [Ability]
getAllAbilities = do
  enemyAbilities <- concatMapM (field EnemyAbilities)
    =<< selectList Matcher.AnyEnemy
  locationAbilities <- concatMapM (field LocationAbilities)
    =<< selectList Matcher.Anywhere
  assetAbilities <- concatMapM (field AssetAbilities)
    =<< selectList Matcher.AnyAsset
  treacheryAbilities <- concatMapM (field TreacheryAbilities)
    =<< selectList Matcher.AnyTreachery
  actAbilities <- concatMapM (field ActAbilities) =<< selectList Matcher.AnyAct
  agendaAbilities <- concatMapM (field AgendaAbilities)
    =<< selectList Matcher.AnyAgenda
  eventAbilities <- concatMapM (field EventAbilities)
    =<< selectList Matcher.AnyEvent
  effectAbilities <- concatMapM (field EffectAbilities)
    =<< selectList Matcher.AnyEffect
  investigatorAbilities <- concatMapM (field InvestigatorAbilities)
    =<< selectList Matcher.Anyone
  pure
    $ enemyAbilities
    <> locationAbilities
    <> assetAbilities
    <> treacheryAbilities
    <> eventAbilities
    <> actAbilities
    <> agendaAbilities
    <> effectAbilities
    <> investigatorAbilities
