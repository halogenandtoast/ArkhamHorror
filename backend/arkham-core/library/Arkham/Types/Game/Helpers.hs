{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Types.Game.Helpers where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Action (Action, TakenAction(..))
import qualified Arkham.Types.Action as Action
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Criteria (Criterion)
import qualified Arkham.Types.Criteria as Criteria
import Arkham.Types.DamageEffect
import Arkham.Types.Deck
import Arkham.Types.Decks
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.GameValue
import Arkham.Types.History
import Arkham.Types.Id
import Arkham.Types.Keyword
import qualified Arkham.Types.Keyword as Keyword
import qualified Arkham.Types.Label as Location
import qualified Arkham.Types.Matcher as Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Phase
import Arkham.Types.Query
import {-# SOURCE #-} Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token
import Arkham.Types.Trait (Trait, toTraits)
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window
import Data.HashSet (size)
import qualified Data.HashSet as HashSet
import Data.UUID (nil)
import System.IO.Unsafe

checkWindows
  :: (MonadReader env m, HasSet InvestigatorId env ())
  => [Window]
  -> m [Message]
checkWindows windows' = do
  iids <- getInvestigatorIds
  pure $ [ CheckWindow iid windows' | iid <- iids ]

windows
  :: (MonadReader env m, HasSet InvestigatorId env ())
  => [Window.WindowType]
  -> m [Message]
windows windows' = do
  iids <- getInvestigatorIds
  pure $ do
    timing <- [Timing.When, Timing.After]
    iid <- iids
    [CheckWindow iid $ map (Window timing) windows']

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

getCanPerformAbility
  :: (MonadReader env m, CanCheckPlayable env, HasCallStack)
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
  :: (MonadReader env m, CanCheckPlayable env)
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
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasCostPayment env
     , HasSet Trait env Source
     , HasList UsedAbility env ()
     )
  => InvestigatorId
  -> Ability
  -> m Bool
getCanAffordAbility iid ability =
  (&&) <$> getCanAffordUse iid ability <*> getCanAffordAbilityCost iid ability

getCanAffordAbilityCost
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasCostPayment env
     , HasSet Trait env Source
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
  :: (MonadReader env m, HasCostPayment env, HasList UsedAbility env ())
  => InvestigatorId
  -> Ability
  -> m Bool
getCanAffordUse iid ability = case abilityLimit ability of
  NoLimit -> case abilityType ability of
    ReactionAbility _ _ ->
      notElem (iid, ability) . map unUsedAbility <$> getList ()
    ForcedAbility _ ->
      notElem (iid, ability) . map unUsedAbility <$> getList ()
    SilentForcedAbility _ ->
      notElem (iid, ability) . map unUsedAbility <$> getList ()
    ForcedAbilityWithCost _ _ ->
      notElem (iid, ability) . map unUsedAbility <$> getList ()
    ActionAbility _ _ -> pure True
    ActionAbilityWithBefore{} -> pure True
    ActionAbilityWithSkill{} -> pure True
    FastAbility _ -> pure True
    AbilityEffect _ -> pure True
    Objective{} -> pure True
  PlayerLimit (PerSearch (Just _)) n ->
    (< n)
      . count ((== abilityLimit ability) . abilityLimit . snd . unUsedAbility)
      <$> getList ()
  PlayerLimit _ n ->
    (< n) . count (== (iid, ability)) . map unUsedAbility <$> getList ()
  PerInvestigatorLimit _ n -> do
    usedAbilities <- map unUsedAbility <$> getList ()
    let
      matchingAbilities = filter (== (iid, ability)) usedAbilities
      matchingPerInvestigatorCount =
        count ((== abilityLimit ability) . abilityLimit . snd) matchingAbilities
    pure $ matchingPerInvestigatorCount < n
  GroupLimit _ n ->
    (< n) . count (== ability) . map (snd . unUsedAbility) <$> getList ()

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
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasCostPayment env
     , HasSet Trait env Source
     , HasCallStack
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
    _ -> error "Not handled"
  ExhaustAssetCost matcher ->
    notNull <$> select (matcher <> Matcher.AssetReady)
  UseCost aid _uType n -> do
    uses <- unUsesCount <$> getCount aid
    pure $ uses >= n
  ActionCost n -> do
    modifiers <- getModifiers source (InvestigatorTarget iid)
    if ActionsAreFree `elem` modifiers
      then pure True
      else do
        takenActions <- map unTakenAction <$> getList iid
        let
          modifiedActionCost =
            foldr (applyActionCostModifier takenActions mAction) n modifiers
        traits <- getSetList @Trait source
        actionCount <- unActionRemainingCount
          <$> getCount (mAction, traits, iid)
        pure $ actionCount >= modifiedActionCost
  ClueCost n -> do
    spendableClues <- unSpendableClueCount <$> getCount iid
    pure $ spendableClues >= n
  PlaceClueOnLocationCost n -> do
    spendableClues <- unSpendableClueCount <$> getCount iid
    pure $ spendableClues >= n
  GroupClueCost n locationMatcher -> do
    cost <- getPlayerCountValue n
    iids <- selectList $ Matcher.InvestigatorAt locationMatcher
    totalSpendableClues <- sum
      <$> for iids ((unSpendableClueCount <$>) . getCount)
    pure $ totalSpendableClues >= cost
  ResourceCost n -> do
    resources <- unResourceCount <$> getCount iid
    pure $ resources >= n
  DiscardFromCost n zone cardMatcher -> do
    -- We need to check that n valid candidates exist across all zones
    -- the logic is that we'll grab all card defs from each zone and then
    -- filter
    let
      getCards = \case
        FromHandOf whoMatcher -> selectList
          (Matcher.InHandOf whoMatcher <> Matcher.BasicCardMatch cardMatcher)
        FromPlayAreaOf whoMatcher ->
          map unInPlayCard <$> (selectList whoMatcher >>= concatMapM getList)
        Zones zs -> concatMapM getCards zs
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
    handCards <- mapMaybe (preview _PlayerCard) <$> getHandOf iid
    let
      total = sum $ map
        (count (`member` insertSet SkillWild skillTypes) . cdSkills . toCardDef)
        handCards
    pure $ total >= n
  HandDiscardCost n mCardType traits skillTypes -> do
    cards <- mapMaybe (preview _PlayerCard) <$> getHandOf iid
    let
      cardTypeFilter = case mCardType of
        Nothing -> const True
        Just cardType' -> (== cardType') . cdCardType . toCardDef
      traitFilter = if null traits
        then const True
        else notNull . intersect traits . toTraits
      skillTypeFilter = if null skillTypes
        then const True
        else
          not
          . null
          . intersect (insertSet SkillWild skillTypes)
          . setFromList
          . cdSkills
          . toCardDef
    pure
      $ length
          (filter
            (and . sequence [traitFilter, cardTypeFilter, skillTypeFilter])
            cards
          )
      >= n

getActions
  :: ( MonadReader env m
     , HasCostPayment env
     , HasList UsedAbility env ()
     , CanCheckPlayable env
     )
  => InvestigatorId
  -> Window
  -> m [Ability]
getActions iid window = do
  actions' <- nub <$> asks getAbilities
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
        AssetSource aid -> insertSet Neutral <$> getSet aid
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
      (getCanAffordAbility iid action)
    )
    actions''
  let forcedActions = filter isForcedAbility actions'''
  pure $ if null forcedActions then actions''' else forcedActions

enemyAtInvestigatorLocation
  :: ( MonadReader env m
     , HasId CardCode env EnemyId
     , HasId LocationId env InvestigatorId
     , HasSet EnemyId env LocationId
     )
  => CardCode
  -> InvestigatorId
  -> m Bool
enemyAtInvestigatorLocation cardCode iid = do
  lid <- getId @LocationId iid
  enemyIds <- getSetList @EnemyId lid
  elem cardCode <$> for enemyIds (getId @CardCode)

getHasRecord :: (HasRecord env, MonadReader env m) => CampaignLogKey -> m Bool
getHasRecord = hasRecord

getRecordCount :: (HasRecord env, MonadReader env m) => CampaignLogKey -> m Int
getRecordCount = hasRecordCount

getRecordSet
  :: (HasRecord env, MonadReader env m)
  => CampaignLogKey
  -> m [Recorded CardCode]
getRecordSet = hasRecordSet

getIsUnused'
  :: (HasList UsedAbility env (), MonadReader env m)
  => InvestigatorId
  -> Ability
  -> m Bool
getIsUnused' iid ability = notElem ability' . map unUsedAbility <$> getList ()
  where ability' = (iid, ability)

getGroupIsUnused
  :: (MonadReader env m, HasList UsedAbility env ()) => Ability -> m Bool
getGroupIsUnused ability =
  notElem ability . map (snd . unUsedAbility) <$> getList ()

getInvestigatorModifiers
  :: (MonadReader env m, HasModifiersFor env ())
  => InvestigatorId
  -> Source
  -> m [ModifierType]
getInvestigatorModifiers iid source =
  getModifiers source (InvestigatorTarget iid)

getXp
  :: ( HasCount XPCount env ()
     , HasModifiersFor env ()
     , HasSet InvestigatorId env ()
     , MonadReader env m
     )
  => m [(InvestigatorId, Int)]
getXp = do
  investigatorIds <- getInvestigatorIds
  for
    investigatorIds
    \iid -> do
      modifiers' <- getModifiers
        (InvestigatorSource iid)
        (InvestigatorTarget iid)
      amount <- unXPCount <$> getCount ()
      pure (iid, foldl' applyModifier amount modifiers')
 where
  applyModifier n (XPModifier m) = max 0 (n + m)
  applyModifier n _ = n

getLeadInvestigatorId
  :: (HasId LeadInvestigatorId env (), MonadReader env m) => m InvestigatorId
getLeadInvestigatorId = unLeadInvestigatorId <$> getId ()

getInvestigatorIds
  :: (HasSet InvestigatorId env (), MonadReader env m) => m [InvestigatorId]
getInvestigatorIds = getSetList ()

getPlayerCount :: (HasCount PlayerCount env (), MonadReader env m) => m Int
getPlayerCount = unPlayerCount <$> getCount ()

getPlayerCountValue
  :: (HasCount PlayerCount env (), MonadReader env m) => GameValue Int -> m Int
getPlayerCountValue gameValue = fromGameValue gameValue <$> getPlayerCount

getLocationSet
  :: (HasSet LocationId env (), MonadReader env m) => m (HashSet LocationId)
getLocationSet = getSet ()

getSpendableClueCount
  :: (MonadReader env m, HasCount SpendableClueCount env InvestigatorId)
  => [InvestigatorId]
  -> m Int
getSpendableClueCount investigatorIds =
  sum <$> for investigatorIds ((unSpendableClueCount <$>) . getCount)

-- TODO: canFight _ a@Attrs {..} = canDo Action.Fight a
getCanFight
  :: ( MonadReader env m
     , HasCostPayment env
     , HasSet InvestigatorId env EnemyId
     , HasSet Keyword env EnemyId
     , HasSet Trait env Source
     , HasId LocationId env InvestigatorId
     , HasId LocationId env EnemyId
     , HasModifiersFor env ()
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanFight eid iid = do
  locationId <- getId @LocationId iid
  enemyModifiers <- getModifiers (InvestigatorSource iid) (EnemyTarget eid)
  sameLocation <- (== locationId) <$> getId @LocationId eid
  modifiers' <- getModifiers (EnemySource eid) (InvestigatorTarget iid)
  takenActions <- setFromList . map unTakenAction <$> getList iid
  keywords <- getSet eid
  canAffordActions <- getCanAffordCost
    iid
    (EnemySource eid)
    (Just Action.Fight)
    []
    (foldl' (applyFightCostModifiers takenActions) (ActionCost 1) modifiers')
  engagedInvestigators <- getSet eid
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
  :: ( MonadReader env m
     , HasCostPayment env
     , HasSet InvestigatorId env EnemyId
     , HasSet Trait env Source
     , HasId LocationId env InvestigatorId
     , HasId LocationId env EnemyId
     , HasModifiersFor env ()
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanEngage eid iid = do
  locationId <- getId @LocationId iid
  sameLocation <- (== locationId) <$> getId @LocationId eid
  notEngaged <- notElem iid <$> getSet eid
  canAffordActions <- getCanAffordCost
    iid
    (EnemySource eid)
    (Just Action.Engage)
    []
    (ActionCost 1)
  pure $ notEngaged && canAffordActions && sameLocation

getCanEvade
  :: ( MonadReader env m
     , HasCostPayment env
     , HasSet InvestigatorId env EnemyId
     , HasSet Trait env Source
     , HasModifiersFor env ()
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanEvade eid iid = do
  engaged <- elem iid <$> getSet eid
  enemyModifiers <- getModifiers (InvestigatorSource iid) (EnemyTarget eid)
  modifiers' <- getModifiers (EnemySource eid) (InvestigatorTarget iid)
  takenActions <- setFromList . map unTakenAction <$> getList iid
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
  :: ( MonadReader env m
     , HasCostPayment env
     , HasSet AccessibleLocationId env LocationId
     , HasSet Trait env Source
     , HasId LocationId env InvestigatorId
     , HasModifiersFor env ()
     , HasHistory env
     , HasCallStack
     )
  => LocationId
  -> InvestigatorId
  -> m Bool
getCanMoveTo lid iid = do
  history <- getHistory TurnHistory iid
  locationId <- getId @LocationId iid
  modifiers' <- getModifiers (LocationSource lid) (InvestigatorTarget iid)
  locationModifiers' <- getModifiers
    (InvestigatorSource iid)
    (LocationTarget lid)
  accessibleLocations <- map unAccessibleLocationId <$> getSetList locationId
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

getResourceCount
  :: (MonadReader env m, HasCount ResourceCount env InvestigatorId)
  => InvestigatorId
  -> m Int
getResourceCount iid = unResourceCount <$> getCount iid

getDiscardOf
  :: (MonadReader env m, HasList DiscardedPlayerCard env InvestigatorId)
  => InvestigatorId
  -> m [PlayerCard]
getDiscardOf iid = map unDiscardedPlayerCard <$> getList iid

getHandOf
  :: (MonadReader env m, HasList HandCard env InvestigatorId)
  => InvestigatorId
  -> m [Card]
getHandOf iid = map unHandCard <$> getList iid

getInPlayOf
  :: (MonadReader env m, HasList InPlayCard env InvestigatorId)
  => InvestigatorId
  -> m [Card]
getInPlayOf iid = map unInPlayCard <$> getList iid

getCardCount
  :: (MonadReader env m, HasCount CardCount env InvestigatorId)
  => InvestigatorId
  -> m Int
getCardCount iid = unCardCount <$> getCount iid

toModifier :: SourceEntity a => a -> ModifierType -> Modifier
toModifier = Modifier . toSource

toModifiers :: SourceEntity a => a -> [ModifierType] -> [Modifier]
toModifiers = map . toModifier

targetToSource :: Target -> Source
targetToSource = \case
  InvestigatorTarget iid -> InvestigatorSource iid
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
  PlayerCardSource cid -> CardIdTarget cid
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
  AttackSource a -> EnemyTarget a
  StorySource code -> StoryTarget code
  InHandSource -> error "not converted"

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
  :: (MonadReader env m, HasId (Maybe LocationId) env Matcher.LocationMatcher)
  => Name
  -> m LocationId
getJustLocationIdByName name =
  fromJustNote ("Missing " <> show name) <$> getLocationIdByName name

getLocationIdByName
  :: (MonadReader env m, HasId (Maybe LocationId) env Matcher.LocationMatcher)
  => Name
  -> m (Maybe LocationId)
getLocationIdByName name = getId matcher
 where
  matcher = case (nameTitle name, nameSubtitle name) of
    (title, Just subtitle) -> Matcher.LocationWithFullTitle title subtitle
    (title, Nothing) -> Matcher.LocationWithTitle title

fightAction :: SourceEntity source => source -> Int -> [Cost] -> Ability
fightAction source n costs =
  mkAbility source n (ActionAbility (Just Action.Fight) (Costs costs))

hasFightActions
  :: (MonadReader env m, Query Matcher.AbilityMatcher env)
  => InvestigatorId
  -> Matcher.WindowMatcher
  -> m Bool
hasFightActions _ window = notNull <$> select
  (Matcher.AbilityIsAction Action.Fight <> Matcher.AbilityWindow window)

hasEvadeActions
  :: (MonadReader env m, Query Matcher.AbilityMatcher env)
  => InvestigatorId
  -> Matcher.WindowMatcher
  -> m Bool
hasEvadeActions _ window = notNull <$> select
  (Matcher.AbilityIsAction Action.Evade <> Matcher.AbilityWindow window)

hasInvestigateActions
  :: (MonadReader env m, Query Matcher.AbilityMatcher env)
  => InvestigatorId
  -> Matcher.WindowMatcher
  -> m Bool
hasInvestigateActions _ window = notNull <$> select
  (Matcher.AbilityIsAction Action.Investigate <> Matcher.AbilityWindow window)

type CanCheckPlayable env
  = ( HasModifiersFor env ()
    , HasCostPayment env
    , HasHistory env
    , HasStep ActStep env ()
    , HasList UnderneathCard env ActDeck
    , HasList UnderneathCard env AgendaDeck
    , HasSet VictoryDisplayCard env ()
    , HasId (Maybe LocationId) env (Direction, LocationId)
    , HasId (Maybe LocationId) env TreacheryId
    , HasId (Maybe LocationId) env EventId
    , ( Query Matcher.AssetMatcher env
      , Query Matcher.EventMatcher env
      , Query Matcher.InvestigatorMatcher env
      , Query Matcher.AbilityMatcher env
      , Query Matcher.LocationMatcher env
      , Query Matcher.TreacheryMatcher env
      , Query Matcher.EnemyMatcher env
      , Query Matcher.SkillMatcher env
      , Query Matcher.ExtendedCardMatcher env
      )
    , HasSkillTest env
    , CanCheckFast env
    , HasSet ClassSymbol env AssetId
    , HasSet SetAsideCardId env Matcher.CardMatcher
    , HasSet TreacheryId env InvestigatorId
    , HasList HandCard env InvestigatorId
    , HasList CommittedSkillIcon env InvestigatorId
    , HasList Card env Matcher.ExtendedCardMatcher
    , HasCount ActionTakenCount env InvestigatorId
    , HasCount HorrorCount env AssetId
    , HasCount HorrorCount env LocationId
    , HasCount ResourceCount env LocationId
    , HasCount ResourceCount env TreacheryId
    , HasCount (Maybe ClueCount) env TreacheryId
    , HasCount ClueCount env AssetId
    , HasCount ClueCount env ActId
    , HasCount ClueCount env InvestigatorId
    , HasCount RemainingSanity env InvestigatorId
    , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
    , HasCount ActionRemainingCount env InvestigatorId
    , HasSet InvestigatorId env LocationId
    , HasSet EnemyId env LocationId
    , HasSet EnemyId env Matcher.EnemyMatcher
    , HasSet LocationId env Matcher.LocationMatcher
    , HasSet Trait env EnemyId
    , HasSet Trait env EnemyId
    , HasCount ClueCount env LocationId
    , HasCount SpendableClueCount env ()
    , HasCount SpendableClueCount env InvestigatorId
    , HasCount UsesCount env AssetId
    , HasId (Maybe LocationId) env Matcher.LocationMatcher
    , HasId LocationId env AssetId
    , HasId (Maybe OwnerId) env AssetId
    , HasId OwnerId env EventId
    , HasId OwnerId env SkillId
    , HasList TakenAction env InvestigatorId
    , HasId CardCode env LocationId
    , HasSet Trait env Source
    , HasAbilities env
    , HasSet EnemyId env InvestigatorId
    , HasCount ResourceCount env InvestigatorId
    , HasCount DoomCount env AssetId
    , HasCount DoomCount env InvestigatorId
    , HasList DiscardedPlayerCard env InvestigatorId
    , HasSet InvestigatorId env Matcher.InvestigatorMatcher
    , HasSet AssetId env Matcher.AssetMatcher
    , HasSet InvestigatorId env ()
    )

getIsPlayable
  :: (HasCallStack, MonadReader env m, CanCheckPlayable env)
  => InvestigatorId
  -> Source
  -> [Window]
  -> Card
  -> m Bool
getIsPlayable iid source windows' c = do
  availableResources <- unResourceCount <$> getCount iid
  getIsPlayableWithResources iid source availableResources windows' c

getIsPlayableWithResources
  :: (HasCallStack, MonadReader env m, CanCheckPlayable env)
  => InvestigatorId
  -> Source
  -> Int
  -> [Window]
  -> Card
  -> m Bool
getIsPlayableWithResources _ _ _ _ (EncounterCard _) = pure False -- TODO: there might be some playable ones?
getIsPlayableWithResources iid source availableResources windows' c@(PlayerCard _)
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
      sum <$> traverse ((fmap unResourceCount . getCount) . fst) canHelpPay
    modifiers <- getModifiers (InvestigatorSource iid) (InvestigatorTarget iid)
    let
      notFastWindow =
        any (`elem` windows') [Window Timing.When (Window.DuringTurn iid)]
    modifiedCardCost <- getModifiedCardCost iid c
    passesCriterias <- maybe
      (pure True)
      (passesCriteria iid source windows')
      (cdCriteria pcDef)
    inFastWindow <- maybe
      (pure False)
      (cardInFastWindows iid source c windows')
      (cdFastWindow pcDef)
    canEvade <- hasEvadeActions iid (Matcher.DuringTurn Matcher.You)
    canFight <- hasFightActions iid (Matcher.DuringTurn Matcher.You)
    passesLimits <- allM passesLimit (cdLimits pcDef)
    pure
      $ (cdCardType pcDef /= SkillType)
      && (modifiedCardCost <= (availableResources + additionalResources))
      && none prevents modifiers
      && ((isNothing (cdFastWindow pcDef) && notFastWindow) || inFastWindow)
      && (cdAction pcDef /= Just Action.Evade || canEvade)
      && (cdAction pcDef /= Just Action.Fight || canFight)
      && passesCriterias
      && passesLimits
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
      n <- size <$> getSet @AssetId
        (Matcher.AssetOwnedBy (Matcher.InvestigatorWithId iid)
        <> Matcher.AssetWithTitle (nameTitle $ toName c)
        )
      pure $ m > n
    _ -> error $ "Not handling card type: " <> show (toCardType c)
  passesLimit (LimitPerTrait t m) = case toCardType c of
    AssetType -> do
      n <- size <$> getSet @AssetId (Matcher.AssetWithTrait t)
      pure $ m > n
    _ -> error $ "Not handling card type: " <> show (toCardType c)

passesCriteria
  :: (HasCallStack, MonadReader env m, CanCheckFast env, CanCheckPlayable env)
  => InvestigatorId
  -> Source
  -> [Window]
  -> Criterion
  -> m Bool
passesCriteria iid source windows' = \case
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
    hand <- map (toCardId . unHandCard) <$> getList iid
    case source of
      EventSource eid -> pure $ unEventId eid `elem` hand
      TreacherySource tid -> do
        member tid <$> select
          (Matcher.TreacheryInHandOf $ Matcher.InvestigatorWithId iid)
      _ -> error $ "source not handled for in your hand: " <> show source
  Criteria.InThreatAreaOf who -> do
    investigators <- selectList who
    case source of
      TreacherySource tid ->
        member tid . HashSet.unions <$> traverse getSet investigators
      EnemySource eid ->
        member eid . HashSet.unions <$> traverse getSet investigators
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
        Criteria.UnderAgendaDeck -> map unUnderneathCard <$> getList AgendaDeck
        Criteria.UnderActDeck -> map unUnderneathCard <$> getList ActDeck
        Criteria.UnderZones zs -> concatMapM getCards zs
    cardCount <- length . filter (`cardMatch` cardMatcher) <$> getCards zone
    gameValueMatches cardCount valueMatcher
  Criteria.SelfHasModifier modifier -> case source of
    InvestigatorSource iid' ->
      elem modifier <$> getModifiers source (InvestigatorTarget iid')
    _ -> pure False
  Criteria.Here -> case source of
    LocationSource lid -> (== lid) <$> getId iid
    ProxySource (LocationSource lid) _ -> (== lid) <$> getId iid
    _ -> pure False
  Criteria.OwnsThis -> case source of
    AssetSource aid -> member aid
      <$> select (Matcher.AssetOwnedBy $ Matcher.InvestigatorWithId iid)
    EventSource eid -> member eid
      <$> select (Matcher.EventOwnedBy $ Matcher.InvestigatorWithId iid)
    _ -> pure False
  Criteria.DuringSkillTest skillTestMatcher -> do
    mSkillTest <- getSkillTest
    case mSkillTest of
      Nothing -> pure False
      Just skillTest -> skillTestMatches iid source skillTest skillTestMatcher
  Criteria.ChargesOnThis valueMatcher -> case source of
    TreacherySource tid ->
      (`gameValueMatches` valueMatcher) . unResourceCount =<< getCount tid
    _ -> error "missing ChargesOnThis check"
  Criteria.ResourcesOnThis valueMatcher -> case source of
    TreacherySource tid ->
      (`gameValueMatches` valueMatcher) . unResourceCount =<< getCount tid
    _ -> error "missing ChargesOnThis check"
  Criteria.ResourcesOnLocation locationMatcher valueMatcher -> do
    locations <- selectList locationMatcher
    total <- sum <$> traverse (fmap unResourceCount . getCount) locations
    gameValueMatches total valueMatcher
  Criteria.CluesOnThis valueMatcher -> case source of
    LocationSource lid -> do
      (`gameValueMatches` valueMatcher) . unClueCount =<< getCount lid
    ActSource aid -> do
      (`gameValueMatches` valueMatcher) . unClueCount =<< getCount aid
    AssetSource aid -> do
      (`gameValueMatches` valueMatcher) . unClueCount =<< getCount aid
    TreacherySource tid -> do
      maybe (pure False) ((`gameValueMatches` valueMatcher) . unClueCount)
        =<< getCount tid
    _ -> error "missing CluesOnThis check"
  Criteria.HorrorOnThis valueMatcher -> case source of
    AssetSource aid -> do
      (`gameValueMatches` valueMatcher) . unHorrorCount =<< getCount aid
    _ -> error $ "missing HorrorOnThis check for " <> show source
  Criteria.Unowned -> case source of
    AssetSource aid -> do
      mOwner <- getId @(Maybe OwnerId) aid
      pure $ isNothing mOwner
    ProxySource (AssetSource aid) _ -> do
      mOwner <- getId @(Maybe OwnerId) aid
      pure $ isNothing mOwner
    _ -> error $ "missing OwnsThis check for source: " <> show source
  Criteria.OnSameLocation -> case source of
    AssetSource aid ->
      liftA2 (==) (getId @LocationId aid) (getId @LocationId iid)
    EnemySource eid ->
      liftA2 (==) (getId @LocationId eid) (getId @LocationId iid)
    TreacherySource tid -> getId tid >>= \case
      Just lid -> (== lid) <$> getId @LocationId iid
      Nothing -> pure False
    ProxySource (AssetSource aid) _ ->
      liftA2 (==) (getId @LocationId aid) (getId @LocationId iid)
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
  Criteria.CardExists cardMatcher -> notNull <$> getList @Card cardMatcher
  Criteria.ExtendedCardExists cardMatcher ->
    notNull <$> getList @Card cardMatcher
  Criteria.PlayableCardExistsWithCostReduction n cardMatcher -> do
    availableResources <- unResourceCount <$> getCount iid
    results <- getList @Card cardMatcher
    anyM
      (getIsPlayableWithResources iid source (availableResources + n) windows')
      results
  Criteria.PlayableCardExists cardMatcher -> do
    results <- getList @Card cardMatcher
    anyM (getIsPlayable iid source windows') results
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
      . concat
      <$> traverse (fmap (map unDiscardedPlayerCard) . getList) investigatorIds
    anyM (getIsPlayable iid source windows'' . PlayerCard) discards
  Criteria.FirstAction -> do
    n <- unActionTakenCount <$> getCount iid
    pure $ n == 0
  Criteria.NoRestriction -> pure True
  Criteria.OnLocation locationMatcher -> do
    lid <- getId @LocationId iid
    anyM
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
    discards <-
      concat
        <$> traverse
              (fmap (map unDiscardedPlayerCard) . getList)
              investigatorIds
    let
      filteredDiscards = case traits of
        [] -> discards
        traitsToMatch ->
          filter (any (`elem` traitsToMatch) . toTraits) discards
    pure $ notNull filteredDiscards
  Criteria.CardInDiscard discardSignifier traits -> do
    let
      investigatorMatcher = case discardSignifier of
        Criteria.DiscardOf matcher -> matcher
        Criteria.AnyPlayerDiscard -> Matcher.Anyone
    investigatorIds <- selectList investigatorMatcher
    discards <-
      concat
        <$> traverse
              (fmap (map unDiscardedPlayerCard) . getList)
              investigatorIds
    let
      filteredDiscards = case traits of
        [] -> discards
        traitsToMatch ->
          filter (any (`elem` traitsToMatch) . toTraits) discards
    pure $ notNull filteredDiscards
  Criteria.ClueOnLocation -> do
    location <- getId iid
    liftA2
      (&&)
      (pure $ location /= LocationId (CardId nil))
      ((> 0) . unClueCount <$> getCount location)
  Criteria.EnemyCriteria enemyCriteria ->
    passesEnemyCriteria iid source windows' enemyCriteria
  Criteria.SetAsideCardExists matcher ->
    notNull <$> getSet @SetAsideCardId matcher
  Criteria.OnAct step -> (== step) . unActStep <$> getStep ()
  Criteria.AssetExists matcher -> notNull <$> getSet @AssetId matcher
  Criteria.InvestigatorExists matcher ->
    -- Because the matcher can't tell who is asking, we need to replace
    -- The You matcher by the Id of the investigator asking
    notNull <$> getSet @InvestigatorId (Matcher.replaceYouMatcher iid matcher)
  Criteria.InvestigatorsHaveSpendableClues valueMatcher ->
    (`gameValueMatches` valueMatcher) . unSpendableClueCount =<< getCount ()
  Criteria.Criteria rs -> allM (passesCriteria iid source windows') rs
  Criteria.AnyCriterion rs -> anyM (passesCriteria iid source windows') rs
  Criteria.LocationExists matcher -> notNull <$> getSet @LocationId matcher
  Criteria.InvestigatorIsAlone -> do
    location <- getId iid
    liftA2
      (&&)
      (pure $ location /= LocationId (CardId nil))
      ((== 1) . size <$> getSet @InvestigatorId location)
  Criteria.InVictoryDisplay cardMatcher valueMatcher -> do
    vCards <-
      filter (`cardMatch` cardMatcher) <$> getSetListMap unVictoryDisplayCard ()
    gameValueMatches (length vCards) valueMatcher
  Criteria.OwnCardWithDoom -> do
    assetIds <- selectList (Matcher.AssetOwnedBy Matcher.You)
    investigatorDoomCount <- unDoomCount <$> getCount iid
    assetsWithDoomCount <- filterM
      (fmap ((> 0) . unDoomCount) . getCount)
      assetIds
    pure $ investigatorDoomCount > 0 || notNull assetsWithDoomCount
  Criteria.ScenarioCardHasResignAbility -> do
    actions' <- asks getAbilities
    pure $ flip
      any
      actions'
      \ability -> case abilityType ability of
        ActionAbility (Just Action.Resign) _ -> True
        _ -> False

-- | Build a matcher and check the list
passesEnemyCriteria
  :: (HasCallStack, MonadReader env m, CanCheckPlayable env)
  => InvestigatorId
  -> Source
  -> [Window]
  -> Criteria.EnemyCriterion
  -> m Bool
passesEnemyCriteria _iid source windows' criterion =
  notNull <$> (getSet @EnemyId =<< matcher criterion)
 where
  matcher = \case
    Criteria.EnemyMatchesCriteria ms -> mconcatMapM matcher ms
    Criteria.EnemyExists m -> pure m
    Criteria.EnemyExistsAtAttachedLocation m -> case source of
      EventSource e -> do
        getId @(Maybe LocationId) e >>= \case
          Nothing -> error "Event must be attached"
          Just lid -> pure $ m <> Matcher.EnemyAt (Matcher.LocationWithId lid)
      _ -> error $ "Does not handle source: " <> show source
    Criteria.ThisEnemy enemyMatcher -> case source of
      EnemySource eid -> pure $ Matcher.EnemyWithId eid <> enemyMatcher
      _ -> error "Invalid source for ThisEnemy"
    Criteria.NotAttackingEnemy ->
      -- TODO: should not be multiple enemies, but if so need to OR not AND matcher
      let
        getAttackingEnemy = \case
          Window _ (Window.EnemyAttacks _ eid) -> Just eid
          _ -> Nothing
      in
        case mapMaybe getAttackingEnemy windows' of
          [] -> error "can not be called without enemy source"
          xs -> pure $ Matcher.NotEnemy (concatMap Matcher.EnemyWithId xs)

getModifiedCardCost
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasList Card env Matcher.CardMatcher
     )
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
    matches <- getList @Card cardMatcher
    pure $ if c `elem` matches then max 0 (n - m) else n
  applyModifier n (IncreaseCostOf cardMatcher m) = do
    matches <- getList @Card cardMatcher
    pure $ if c `elem` matches then n + m else n
  applyModifier n _ = pure n
getModifiedCardCost iid c@(EncounterCard _) = do
  modifiers <- getModifiers (InvestigatorSource iid) (InvestigatorTarget iid)
  foldM
    applyModifier
    (error "we need so specify ecCost for this to work")
    modifiers
 where
  applyModifier n (ReduceCostOf cardMatcher m) = do
    matches <- getList @Card cardMatcher
    pure $ if c `elem` matches then max 0 (n - m) else n
  applyModifier n (IncreaseCostOf cardMatcher m) = do
    matches <- getList @Card cardMatcher
    pure $ if c `elem` matches then n + m else n
  applyModifier n _ = pure n

type CanCheckFast env
  = ( HasSet Trait env EnemyId
    , HasSet InvestigatorId env Matcher.Who
    , HasList UnderneathCard env InvestigatorId
    , HasList Card env Matcher.CardMatcher
    , HasCount DamageCount env InvestigatorId
    , HasCount HorrorCount env InvestigatorId
    , HasCount ClueCount env LocationId
    , HasSet AccessibleLocationId env LocationId
    , HasSet ConnectedLocationId env LocationId
    , HasSet InvestigatorId env LocationId
    , HasSet RevealedLocationId env ()
    , HasSet InvestigatorId env EnemyId
    , HasSet EnemyId env LocationId
    , HasSet TreacheryId env LocationId
    , HasId LocationId env InvestigatorId
    , HasId LocationId env EnemyId
    , HasId CardCode env TreacheryId
    , HasId CardCode env EnemyId
    , HasSet Trait env LocationId
    , HasSet Keyword env EnemyId
    , HasSet FarthestLocationId env (InvestigatorId, Matcher.LocationMatcher)
    , HasSet ClosestLocationId env (InvestigatorId, Matcher.LocationMatcher)
    , HasName env LocationId
    , HasName env (Unrevealed LocationId)
    , HasName env EnemyId
    , HasCount PlayerCount env ()
    , Location.GetLabel env LocationId
    , HasTokenValue env ()
    )

depthGuard :: IORef Int
depthGuard = unsafePerformIO $ newIORef 0
{-# NOINLINE depthGuard #-}

cardInFastWindows
  :: (MonadReader env m, CanCheckPlayable env)
  => InvestigatorId
  -> Source
  -> Card
  -> [Window]
  -> Matcher.WindowMatcher
  -> m Bool
cardInFastWindows iid source _ windows' matcher =
  anyM (\window -> windowMatches iid source window matcher) windows'

windowMatches
  :: (MonadReader env m, CanCheckPlayable env)
  => InvestigatorId
  -> Source
  -> Window
  -> Matcher.WindowMatcher
  -> m Bool
windowMatches iid source window' = \case
  Matcher.AnyWindow -> pure True
  Matcher.DrawingStartingHand timing whoMatcher -> case window' of
    Window t (Window.DrawingStartingHand who) | t == timing ->
      matchWho iid who whoMatcher
    _ -> pure False
  Matcher.MovedFromHunter timing enemyMatcher -> case window' of
    Window t (Window.MovedFromHunter eid) | t == timing ->
      member eid <$> select enemyMatcher
    _ -> pure False
  Matcher.CommittedCard timing whoMatcher cardMatcher -> case window' of
    Window t (Window.CommittedCard who card) | t == timing -> liftA2
      (&&)
      (matchWho iid who whoMatcher)
      (pure $ cardMatch card cardMatcher)
    _ -> pure False
  Matcher.CommittedCards timing whoMatcher cardListMatcher -> case window' of
    Window t (Window.CommittedCards who cards) | t == timing -> liftA2
      (&&)
      (matchWho iid who whoMatcher)
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
    Window t (Window.TookControlOfAsset who aid) | t == timing -> liftA2
      (&&)
      (matchWho iid who whoMatcher)
      (member aid <$> select assetMatcher)
    _ -> pure False
  Matcher.WouldDrawEncounterCard timing whoMatcher -> case window' of
    Window t (Window.WouldDrawEncounterCard who) | t == timing ->
      matchWho iid who whoMatcher
    _ -> pure False
  Matcher.AmongSearchedCards whoMatcher -> case window' of
    Window _ (Window.AmongSearchedCards who) -> matchWho iid who whoMatcher
    _ -> pure False
  Matcher.Discarded timing whoMatcher cardMatcher -> case window' of
    Window t (Window.Discarded who card) | t == timing ->
      (cardMatch card cardMatcher &&) <$> matchWho iid who whoMatcher
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
      pure $ agendaMatches aid agendaMatcher
    _ -> pure False
  Matcher.MovedBy timingMatcher whoMatcher sourceMatcher -> case window' of
    Window t (Window.MovedBy source' _ who) | t == timingMatcher -> liftA2
      (&&)
      (matchWho iid who whoMatcher)
      (sourceMatches source' sourceMatcher)
    _ -> pure False
  Matcher.InvestigatorDefeated timingMatcher sourceMatcher whoMatcher ->
    case window' of
      Window t (Window.InvestigatorDefeated source' who) | t == timingMatcher ->
        liftA2
          (&&)
          (matchWho iid who whoMatcher)
          (sourceMatches source' sourceMatcher)
      _ -> pure False
  Matcher.AgendaWouldAdvance timingMatcher advancementReason agendaMatcher ->
    case window' of
      Window t (Window.AgendaWouldAdvance advancementReason' aid)
        | t == timingMatcher && advancementReason == advancementReason'
        -> pure $ agendaMatches aid agendaMatcher
      _ -> pure False
  Matcher.PlacedCounter whenMatcher whoMatcher counterMatcher valueMatcher ->
    case window' of
      Window t (Window.PlacedHorror iid' n)
        | t == whenMatcher && counterMatcher == Matcher.HorrorCounter -> liftA2
          (&&)
          (matchWho iid iid' whoMatcher)
          (gameValueMatches n valueMatcher)
      Window t (Window.PlacedDamage iid' n)
        | t == whenMatcher && counterMatcher == Matcher.DamageCounter -> liftA2
          (&&)
          (matchWho iid iid' whoMatcher)
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
          (matchWho iid who whoMatcher)
          (locationMatches iid source window' locationId locationMatcher)
      _ -> pure False
  Matcher.GameEnds timingMatcher -> case window' of
    Window t Window.EndOfGame -> pure $ t == timingMatcher
    _ -> pure False
  Matcher.InvestigatorEliminated timingMatcher whoMatcher -> case window' of
    Window t (Window.InvestigatorEliminated who) | t == timingMatcher ->
      matchWho iid who whoMatcher
    _ -> pure False
  Matcher.PutLocationIntoPlay timingMatcher whoMatcher locationMatcher ->
    case window' of
      Window t (Window.PutLocationIntoPlay who locationId)
        | t == timingMatcher -> liftA2
          (&&)
          (matchWho iid who whoMatcher)
          (locationMatches iid source window' locationId locationMatcher)
      _ -> pure False
  Matcher.PlayerHasPlayableCard cardMatcher -> do
    -- TODO: do we need to grab the card source?
    -- cards <- filter (/= c) <$> getList cardMatcher
    cards <- getList cardMatcher
    anyM (getIsPlayable iid source [window']) cards
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
      matchWho iid who whoMatcher
    _ -> pure False
  Matcher.TurnEnds whenMatcher whoMatcher -> case window' of
    Window t (Window.TurnEnds who) | t == whenMatcher ->
      matchWho iid who whoMatcher
    _ -> pure False
  Matcher.RoundEnds whenMatcher -> case window' of
    Window t Window.AtEndOfRound -> pure $ t == whenMatcher
    _ -> pure False
  Matcher.Enters whenMatcher whoMatcher whereMatcher -> case window' of
    Window t (Window.Entering iid' lid) | whenMatcher == t -> liftA2
      (&&)
      (matchWho iid iid' whoMatcher)
      (locationMatches iid source window' lid whereMatcher)
    _ -> pure False
  Matcher.Leaves whenMatcher whoMatcher whereMatcher -> case window' of
    Window t (Window.Leaving iid' lid) | whenMatcher == t -> liftA2
      (&&)
      (matchWho iid iid' whoMatcher)
      (locationMatches iid source window' lid whereMatcher)
    _ -> pure False
  Matcher.Moves whenMatcher whoMatcher fromMatcher toMatcher -> case window' of
    Window t (Window.Moves iid' fromLid toLid) | whenMatcher == t -> andM
      [ matchWho iid iid' whoMatcher
      , locationMatches iid source window' fromLid fromMatcher
      , locationMatches iid source window' toLid toMatcher
      ]
    _ -> pure False
  Matcher.MoveAction whenMatcher whoMatcher fromMatcher toMatcher ->
    case window' of
      Window t (Window.MoveAction iid' fromLid toLid) | whenMatcher == t -> andM
        [ matchWho iid iid' whoMatcher
        , locationMatches iid source window' fromLid fromMatcher
        , locationMatches iid source window' toLid toMatcher
        ]
      _ -> pure False
  Matcher.PerformAction whenMatcher whoMatcher actionMatcher -> case window' of
    Window t (Window.PerformAction iid' action) | whenMatcher == t ->
      andM [matchWho iid iid' whoMatcher, actionMatches action actionMatcher]
    _ -> pure False
  Matcher.WouldHaveSkillTestResult whenMatcher whoMatcher _ skillTestResultMatcher
    -> case skillTestResultMatcher of
      Matcher.FailureResult _ -> case window' of
        Window t (Window.WouldFailSkillTest who) | t == whenMatcher ->
          matchWho iid who whoMatcher
        _ -> pure False
      Matcher.SuccessResult _ -> pure False -- no pass window exists yet, add below too if added
      Matcher.AnyResult -> case window' of
        Window Timing.When (Window.WouldFailSkillTest who) ->
          matchWho iid who whoMatcher
        -- TODO: Add success window if it exists
        _ -> pure False
  Matcher.InitiatedSkillTest whenMatcher whoMatcher _ valueMatcher ->
    case window' of
      Window t (Window.InitiatedSkillTest who _ difficulty)
        | t == whenMatcher -> liftA2
          (&&)
          (matchWho iid who whoMatcher)
          (gameValueMatches difficulty valueMatcher)
      _ -> pure False
  Matcher.SkillTestResult whenMatcher whoMatcher skillMatcher skillTestResultMatcher
    -> case skillTestResultMatcher of
      Matcher.FailureResult gameValueMatcher -> case window' of
        Window t (Window.FailInvestigationSkillTest who lid n)
          | whenMatcher == t -> case skillMatcher of
            Matcher.WhileInvestigating whereMatcher -> andM
              [ matchWho iid who whoMatcher
              , gameValueMatches n gameValueMatcher
              , locationMatches iid source window' lid whereMatcher
              ]
            _ -> pure False
        Window t (Window.FailAttackEnemy who enemyId n) | whenMatcher == t ->
          case skillMatcher of
            Matcher.WhileAttackingAnEnemy enemyMatcher -> andM
              [ matchWho iid who whoMatcher
              , gameValueMatches n gameValueMatcher
              , enemyMatches enemyId enemyMatcher
              ]
            _ -> pure False
        Window t (Window.FailEvadeEnemy who enemyId n) | whenMatcher == t ->
          case skillMatcher of
            Matcher.WhileEvadingAnEnemy enemyMatcher -> andM
              [ matchWho iid who whoMatcher
              , gameValueMatches n gameValueMatcher
              , enemyMatches enemyId enemyMatcher
              ]
            _ -> pure False
        Window t (Window.FailSkillTest who n)
          | whenMatcher == t && skillMatcher == Matcher.AnySkillTest -> liftA2
            (&&)
            (matchWho iid who whoMatcher)
            (gameValueMatches n gameValueMatcher)
        _ -> pure False
      Matcher.SuccessResult gameValueMatcher -> case window' of
        Window t (Window.PassInvestigationSkillTest who lid n)
          | whenMatcher == t -> case skillMatcher of
            Matcher.WhileInvestigating whereMatcher -> andM
              [ matchWho iid who whoMatcher
              , gameValueMatches n gameValueMatcher
              , locationMatches iid source window' lid whereMatcher
              ]
            _ -> pure False
        Window t (Window.SuccessfulAttackEnemy who enemyId n)
          | whenMatcher == t -> case skillMatcher of
            Matcher.WhileAttackingAnEnemy enemyMatcher -> andM
              [ matchWho iid who whoMatcher
              , gameValueMatches n gameValueMatcher
              , enemyMatches enemyId enemyMatcher
              ]
            _ -> pure False
        Window t (Window.SuccessfulEvadeEnemy who enemyId n)
          | whenMatcher == t -> case skillMatcher of
            Matcher.WhileEvadingAnEnemy enemyMatcher -> andM
              [ matchWho iid who whoMatcher
              , gameValueMatches n gameValueMatcher
              , enemyMatches enemyId enemyMatcher
              ]
            _ -> pure False
        Window t (Window.PassSkillTest _ _ who n)
          | whenMatcher == t && skillMatcher == Matcher.AnySkillTest -> liftA2
            (&&)
            (matchWho iid who whoMatcher)
            (gameValueMatches n gameValueMatcher)
        _ -> pure False
      Matcher.AnyResult -> case window' of
        Window t (Window.FailSkillTest who _) | whenMatcher == t ->
          matchWho iid who whoMatcher
        Window t (Window.PassSkillTest _ _ who _) | whenMatcher == t ->
          matchWho iid who whoMatcher
        _ -> pure False
  Matcher.DuringTurn whoMatcher -> case window' of
    Window Timing.When Window.NonFast -> matchWho iid iid whoMatcher
    Window Timing.When (Window.DuringTurn who) -> matchWho iid who whoMatcher
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
  Matcher.EnemyWouldAttack timingMatcher whoMatcher enemyMatcher ->
    case window' of
      Window t (Window.EnemyWouldAttack who enemyId) | timingMatcher == t ->
        liftA2
          (&&)
          (matchWho iid who whoMatcher)
          (enemyMatches enemyId enemyMatcher)
      _ -> pure False
  Matcher.EnemyAttacks timingMatcher whoMatcher enemyMatcher -> case window' of
    Window t (Window.EnemyAttacks who enemyId) | timingMatcher == t -> liftA2
      (&&)
      (matchWho iid who whoMatcher)
      (enemyMatches enemyId enemyMatcher)
    _ -> pure False
  Matcher.EnemyAttacked timingMatcher whoMatcher sourceMatcher enemyMatcher ->
    case window' of
      Window t (Window.EnemyAttacked who attackSource enemyId)
        | timingMatcher == t -> andM
          [ matchWho iid who whoMatcher
          , enemyMatches enemyId enemyMatcher
          , sourceMatches attackSource sourceMatcher
          ]
      _ -> pure False
  Matcher.EnemyEvaded timingMatcher whoMatcher enemyMatcher -> case window' of
    Window t (Window.EnemyEvaded who enemyId) | timingMatcher == t -> liftA2
      (&&)
      (enemyMatches enemyId enemyMatcher)
      (matchWho iid who whoMatcher)
    _ -> pure False
  Matcher.EnemyEngaged timingMatcher whoMatcher enemyMatcher -> case window' of
    Window t (Window.EnemyEngaged who enemyId) | timingMatcher == t -> liftA2
      (&&)
      (enemyMatches enemyId enemyMatcher)
      (matchWho iid who whoMatcher)
    _ -> pure False
  Matcher.MythosStep mythosStepMatcher -> case window' of
    Window t Window.AllDrawEncounterCard | t == Timing.When ->
      pure $ mythosStepMatcher == Matcher.WhenAllDrawEncounterCard
    _ -> pure False
  Matcher.WouldRevealChaosToken whenMatcher whoMatcher -> case window' of
    Window t (Window.WouldRevealChaosToken _ who) | whenMatcher == t ->
      matchWho iid who whoMatcher
    _ -> pure False
  Matcher.RevealChaosToken whenMatcher whoMatcher tokenMatcher ->
    case window' of
      Window t (Window.RevealToken who token) | whenMatcher == t -> liftA2
        (&&)
        (matchWho iid who whoMatcher)
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
        (matchWho iid who whoMatcher)
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
      | t == whenMatcher -> matchWho iid iid' whoMatcher
    _ -> pure False
  Matcher.DealtHorror whenMatcher whoMatcher -> case window' of
    Window t (Window.DealtHorror _ (InvestigatorTarget iid'))
      | t == whenMatcher -> matchWho iid iid' whoMatcher
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
  Matcher.DiscoverClues whenMatcher whoMatcher whereMatcher valueMatcher ->
    case window' of
      Window t (Window.DiscoverClues who lid n) | whenMatcher == t -> andM
        [ matchWho iid who whoMatcher
        , locationMatches iid source window' lid whereMatcher
        , gameValueMatches n valueMatcher
        ]
      _ -> pure False
  Matcher.GainsClues whenMatcher whoMatcher valueMatcher -> case window' of
    Window t (Window.GainsClues who n) | whenMatcher == t ->
      andM [matchWho iid who whoMatcher, gameValueMatches n valueMatcher]
    _ -> pure False
  Matcher.DiscoveringLastClue whenMatcher whoMatcher whereMatcher ->
    case window' of
      Window t (Window.DiscoveringLastClue who lid) | whenMatcher == t -> liftA2
        (&&)
        (matchWho iid who whoMatcher)
        (locationMatches iid source window' lid whereMatcher)
      _ -> pure False
  Matcher.LastClueRemovedFromAsset whenMatcher assetMatcher -> case window' of
    Window t (Window.LastClueRemovedFromAsset aid) | whenMatcher == t ->
      member aid <$> select assetMatcher
    _ -> pure False
  Matcher.DrawCard whenMatcher whoMatcher cardMatcher deckMatcher ->
    case window' of
      Window t (Window.DrawCard who card deck) | whenMatcher == t -> andM
        [ matchWho iid who whoMatcher
        , case cardMatcher of
          Matcher.BasicCardMatch baseMatcher ->
            pure $ cardMatch card baseMatcher
          _ -> member card <$> select cardMatcher
        , deckMatch iid deck deckMatcher
        ]
      _ -> pure False
  Matcher.DeckHasNoCards whenMatcher whoMatcher -> case window' of
    Window t (Window.DeckHasNoCards who) | whenMatcher == t ->
      matchWho iid who whoMatcher
    _ -> pure False
  Matcher.PlayCard whenMatcher whoMatcher cardMatcher -> case window' of
    Window t (Window.PlayCard who card) | whenMatcher == t -> liftA2
      (&&)
      (matchWho iid who whoMatcher)
      (member card <$> select cardMatcher)
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
  :: (MonadReader env m, CanCheckPlayable env)
  => InvestigatorId
  -> InvestigatorId
  -> Matcher.InvestigatorMatcher
  -> m Bool
matchWho you who = \case
  Matcher.Anyone -> pure True
  Matcher.You -> pure $ who == you
  Matcher.NotYou -> pure $ who /= you
  Matcher.UnengagedInvestigator -> null <$> getSet @EnemyId who
  Matcher.NoDamageDealtThisTurn ->
    null . historyDealtDamageTo <$> getHistory TurnHistory who
  Matcher.TurnInvestigator -> do
    mTurn <- selectOne Matcher.TurnInvestigator
    pure $ Just who == mTurn
  Matcher.HandWith cardListMatcher -> do
    hand <- map unHandCard <$> getList who
    cardListMatches hand cardListMatcher
  Matcher.DiscardWith cardListMatcher -> do
    discard' <- map (PlayerCard . unDiscardedPlayerCard) <$> getList who
    cardListMatches discard' cardListMatcher
  Matcher.ContributedMatchingIcons valueMatcher -> do
    icons <- getList @CommittedSkillIcon who
    gameValueMatches (length icons) valueMatcher
  Matcher.InvestigatorWithoutModifier modifier' ->
    notElem modifier'
      <$> getModifiers (InvestigatorSource who) (InvestigatorTarget who)
  Matcher.UneliminatedInvestigator ->
    member who <$> getSet Matcher.UneliminatedInvestigator
  Matcher.ResignedInvestigator ->
    member who <$> getSet Matcher.ResignedInvestigator
  Matcher.InvestigatorEngagedWith enemyMatcher -> do
    engagedInvestigators <- select
      (Matcher.InvestigatorEngagedWith enemyMatcher)
    pure $ who `member` engagedInvestigators
  Matcher.InvestigatorAt locationMatcher -> do
    lid <- getId @LocationId who
    member lid <$> select locationMatcher
  Matcher.InvestigatorWithTitle title -> do
    member who <$> select (Matcher.InvestigatorWithTitle title)
  Matcher.InvestigatorCanMove -> do
    notElem CannotMove
      <$> getModifiers (InvestigatorSource who) (InvestigatorTarget who)
  Matcher.InvestigatorWithActionsRemaining valueMatcher ->
    (`gameValueMatches` valueMatcher) . unActionRemainingCount =<< getCount who
  Matcher.InvestigatorWithClues valueMatcher ->
    (`gameValueMatches` valueMatcher) . unClueCount =<< getCount who
  Matcher.InvestigatorWithDamage valueMatcher ->
    (`gameValueMatches` valueMatcher) . unDamageCount =<< getCount who
  Matcher.InvestigatorWithHorror valueMatcher ->
    (`gameValueMatches` valueMatcher) . unHorrorCount =<< getCount who
  Matcher.InvestigatorWithRemainingSanity valueMatcher ->
    (`gameValueMatches` valueMatcher) . unRemainingSanity =<< getCount who
  Matcher.InvestigatorWithResources valueMatcher ->
    (`gameValueMatches` valueMatcher) . unResourceCount =<< getCount who
  Matcher.InvestigatorWithId iid' -> pure $ who == iid'
  Matcher.InvestigatorMatches is -> allM (matchWho you who) is
  Matcher.AnyInvestigator is -> anyM (matchWho you who) is

gameValueMatches
  :: (MonadReader env m, HasCount PlayerCount env ())
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

sourceMatches
  :: ( MonadReader env m
     , HasSet Trait env Source
     , HasId (Maybe OwnerId) env AssetId
     , HasId OwnerId env EventId
     , HasId OwnerId env SkillId
     , Query Matcher.InvestigatorMatcher env
     )
  => Source
  -> Matcher.SourceMatcher
  -> m Bool
sourceMatches s = \case
  Matcher.SourceMatchesAny ms -> anyM (sourceMatches s) ms
  Matcher.SourceWithTrait t -> elem t <$> getSet s
  Matcher.SourceIs s' -> pure $ s == s'
  Matcher.AnySource -> pure True
  Matcher.SourceMatches ms -> allM (sourceMatches s) ms
  Matcher.SourceOwnedBy whoMatcher -> case s of
    AssetSource aid -> do
      mOwnerId <- fmap unOwnerId <$> getId aid
      case mOwnerId of
        Just iid' -> member iid' <$> select whoMatcher
        _ -> pure False
    EventSource eid -> do
      mOwnerId <- unOwnerId <$> getId eid
      member mOwnerId <$> select whoMatcher
    SkillSource sid -> do
      mOwnerId <- unOwnerId <$> getId sid
      member mOwnerId <$> select whoMatcher
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
  Matcher.EncounterCardSource -> pure $ case s of
    ActSource _ -> True
    AgendaSource _ -> True
    EnemySource _ -> True
    LocationSource _ -> True
    TreacherySource _ -> True
    _ -> False

enemyMatches
  :: (MonadReader env m, CanCheckPlayable env)
  => EnemyId
  -> Matcher.EnemyMatcher
  -> m Bool
enemyMatches !enemyId !mtchr = member enemyId <$> getSet mtchr

locationMatches
  :: (MonadReader env m, CanCheckPlayable env)
  => InvestigatorId
  -> Source
  -> Window
  -> LocationId
  -> Matcher.LocationMatcher
  -> m Bool
locationMatches investigatorId source window locationId = \case
  Matcher.LocationNotInPlay -> pure False
  Matcher.LocationWithLabel label ->
    (== label) . Location.unLabel <$> Location.getLabel locationId
  Matcher.LocationWithTitle title ->
    (== title) . nameTitle <$> getName locationId
  Matcher.LocationWithFullTitle title subtitle ->
    (== Name title (Just subtitle)) <$> getName locationId
  Matcher.LocationWithSymbol locationSymbol ->
    member locationId <$> select (Matcher.LocationWithSymbol locationSymbol)
  Matcher.LocationWithUnrevealedTitle title ->
    (== title) . nameTitle <$> getName (Unrevealed locationId)
  Matcher.LocationWithId lid -> pure $ lid == locationId
  Matcher.LocationIs cardCode -> (== cardCode) <$> getId locationId
  Matcher.Anywhere -> pure True
  Matcher.Unblocked -> notElem Blocked <$> getModifiers
    (InvestigatorSource investigatorId)
    (LocationTarget locationId)
  Matcher.EmptyLocation -> liftA2
    (&&)
    (null <$> getSet @EnemyId locationId)
    (null <$> getSet @InvestigatorId locationId)
  Matcher.LocationWithoutInvestigators ->
    null <$> getSet @InvestigatorId locationId
  Matcher.LocationWithoutEnemies -> null <$> getSet @EnemyId locationId
  Matcher.LocationWithEnemy enemyMatcher -> notNull <$> select
    (Matcher.EnemyAt (Matcher.LocationWithId locationId) <> enemyMatcher)
  Matcher.LocationWithAsset assetMatcher -> notNull <$> select
    (Matcher.AssetAt (Matcher.LocationWithId locationId) <> assetMatcher)
  Matcher.LocationWithInvestigator whoMatcher -> notNull <$> select
    (Matcher.InvestigatorAt (Matcher.LocationWithId locationId) <> whoMatcher)
  Matcher.AccessibleLocation -> do
    yourLocationId <- getId @LocationId investigatorId
    member (AccessibleLocationId locationId) <$> getSet yourLocationId
  Matcher.AccessibleFrom locationMatcher -> do
    lids <- selectList locationMatcher
    anyM (fmap (member (AccessibleLocationId locationId)) . getSet) lids
  Matcher.AccessibleTo locationMatcher -> do
    accessibleLocations <- map unAccessibleLocationId <$> getSetList locationId
    destinations <- select locationMatcher
    pure $ notNull $ intersect (setFromList accessibleLocations) destinations
  Matcher.ConnectedLocation -> do
    yourLocationId <- getId @LocationId investigatorId
    member (ConnectedLocationId locationId) <$> getSet yourLocationId
  Matcher.RevealedLocation ->
    member locationId <$> select Matcher.RevealedLocation
  Matcher.UnrevealedLocation ->
    member locationId <$> select Matcher.UnrevealedLocation
  Matcher.LocationWithClues valueMatcher ->
    (`gameValueMatches` valueMatcher) . unClueCount =<< getCount locationId
  Matcher.LocationWithHorror valueMatcher ->
    (`gameValueMatches` valueMatcher) . unHorrorCount =<< getCount locationId
  Matcher.LocationWithMostClues locationMatcher ->
    member locationId <$> select (Matcher.LocationWithMostClues locationMatcher)
  Matcher.LocationWithResources valueMatcher ->
    (`gameValueMatches` valueMatcher) . unResourceCount =<< getCount locationId
  Matcher.LocationLeavingPlay -> case window of
    Window _ (Window.LeavePlay (LocationTarget lid)) ->
      pure $ locationId == lid
    _ -> error "invalid window for LocationLeavingPlay"
  Matcher.SameLocation -> do
    lid' <- case source of
      EnemySource eid -> getId @LocationId eid
      AssetSource eid -> getId @LocationId eid
      _ -> error $ "can't detect same location for source " <> show source
    pure $ locationId == lid'
  Matcher.YourLocation -> do
    yourLocationId <- getId @LocationId investigatorId
    pure $ locationId == yourLocationId
  Matcher.ThisLocation -> case source of
    (LocationSource lid) -> pure $ lid == locationId
    (ProxySource (LocationSource lid) _) -> pure $ lid == locationId
    _ -> error "Invalid source for ThisLocation"
  Matcher.NotYourLocation -> do
    yourLocationId <- getId @LocationId investigatorId
    pure $ locationId /= yourLocationId
  Matcher.LocationInDirection direction matcher' -> do
    starts <- selectList matcher'
    elem locationId . catMaybes <$> traverse (getId . (direction, )) starts
  Matcher.FarthestLocationFromYou matcher' ->
    member (FarthestLocationId locationId) <$> getSet (investigatorId, matcher')
  Matcher.FarthestLocationFromAll matcher' -> do
    member locationId <$> select (Matcher.FarthestLocationFromAll matcher')
  Matcher.NearestLocationToYou matcher' ->
    member (ClosestLocationId locationId) <$> getSet (investigatorId, matcher')
  Matcher.LocationWithTrait t -> member t <$> getSet locationId
  Matcher.LocationWithoutTrait t -> notMember t <$> getSet locationId
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
  :: ( MonadReader env m
     , HasId LocationId env InvestigatorId
     , HasSet Trait env Source
     , Query Matcher.SkillMatcher env
     , Query Matcher.EnemyMatcher env
     , Query Matcher.LocationMatcher env
     , Query Matcher.TreacheryMatcher env
     , Query Matcher.InvestigatorMatcher env
     , HasId (Maybe OwnerId) env AssetId
     , HasId OwnerId env EventId
     , HasId OwnerId env SkillId
     )
  => InvestigatorId
  -> Source
  -> SkillTest
  -> Matcher.SkillTestMatcher
  -> m Bool
skillTestMatches iid source st = \case
  Matcher.AnySkillTest -> pure True
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
  Matcher.SkillTestAtYourLocation -> liftA2
    (==)
    (getId @LocationId iid)
    (getId @LocationId $ skillTestInvestigator st)
  Matcher.SkillTestMatches ms -> allM (skillTestMatches iid source st) ms

matchToken
  :: (HasTokenValue env (), MonadReader env m)
  => InvestigatorId
  -> Token
  -> Matcher.TokenMatcher
  -> m Bool
matchToken iid' t = \case
  Matcher.WithNegativeModifier -> do
    tv <- getTokenValue () iid' (tokenFace t)
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
  :: (SourceEntity source, MonadReader env m, HasModifiersFor env ())
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
  :: (MonadReader env m, HasCount PlayerCount env ())
  => [Card]
  -> Matcher.CardListMatcher
  -> m Bool
cardListMatches cards = \case
  Matcher.AnyCards -> pure True
  Matcher.LengthIs valueMatcher -> gameValueMatches (length cards) valueMatcher
  Matcher.HasCard cardMatcher -> pure $ any (`cardMatch` cardMatcher) cards

deckMatch
  :: (MonadReader env m, CanCheckPlayable env)
  => InvestigatorId
  -> DeckSignifier
  -> Matcher.DeckMatcher
  -> m Bool
deckMatch iid deckSignifier = \case
  Matcher.EncounterDeck -> pure $ deckSignifier == EncounterDeck
  Matcher.DeckOf investigatorMatcher -> matchWho iid iid investigatorMatcher
  Matcher.AnyDeck -> pure True

agendaMatches :: AgendaId -> Matcher.AgendaMatcher -> Bool
agendaMatches _ Matcher.AnyAgenda = True
agendaMatches aid (Matcher.AgendaWithId aid') = aid == aid'

actionMatches :: Applicative m => Action -> Matcher.ActionMatcher -> m Bool
actionMatches a (Matcher.ActionIs a') = pure $ a == a'

damageEffectMatches
  :: Applicative m => DamageEffect -> Matcher.DamageEffectMatcher -> m Bool
damageEffectMatches a = \case
  Matcher.AnyDamageEffect -> pure True
  Matcher.AttackDamageEffect -> pure $ a == AttackDamageEffect
  Matcher.NonAttackDamageEffect -> pure $ a == NonAttackDamageEffect

spawnAtOneOf
  :: (MonadIO m, HasSet LocationId env (), MonadReader env m, HasQueue env)
  => InvestigatorId
  -> EnemyId
  -> [LocationId]
  -> m ()
spawnAtOneOf iid eid targetLids = do
  locations' <- getSet ()
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
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasSet Trait env Source
     , HasId (Maybe OwnerId) env AssetId
     , HasId OwnerId env EventId
     , HasId OwnerId env SkillId
     , Query Matcher.InvestigatorMatcher env
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
