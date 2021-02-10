{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arkham.Types.Game.Helpers where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Action hiding (Ability)
import qualified Arkham.Types.Action as Action
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.PlayerCard hiding (traits)
import Arkham.Types.Keyword
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Trait (Trait)

cancelToken :: (HasQueue env, MonadReader env m, MonadIO m) => Token -> m ()
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

replaceToken :: (HasQueue env, MonadReader env m, MonadIO m) => Token -> m ()
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

withBaseActions
  :: (MonadIO m, HasActions env a, MonadReader env m)
  => InvestigatorId
  -> Window
  -> a
  -> m [Message]
  -> m [Message]
withBaseActions iid window a f = (<>) <$> getActions iid window a <*> f

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
  ActionAbility mAction cost -> getCanAffordCost iid abilitySource mAction cost
  ReactionAbility cost -> getCanAffordCost iid abilitySource Nothing cost
  FastAbility cost -> getCanAffordCost iid abilitySource Nothing cost
  ForcedAbility -> pure True

getCanAffordUse
  :: (MonadReader env m, HasCostPayment env, HasList UsedAbility env ())
  => InvestigatorId
  -> Ability
  -> m Bool
getCanAffordUse iid ability = case abilityLimit ability of
  NoLimit -> case abilityType ability of
    ReactionAbility _ ->
      notElem (iid, ability) . map unUsedAbility <$> getList ()
    ForcedAbility -> notElem (iid, ability) . map unUsedAbility <$> getList ()
    ActionAbility _ _ -> pure True
    FastAbility _ -> pure True
  PlayerLimit (PerSearch (Just _)) n ->
    (< n)
      . count ((== abilityLimit ability) . abilityLimit . snd . unUsedAbility)
      <$> getList ()
  PlayerLimit _ n ->
    (< n) . count (== (iid, ability)) . map unUsedAbility <$> getList ()
  GroupLimit _ n ->
    (< n) . count (== ability) . map (snd . unUsedAbility) <$> getList ()

applyActionCostModifier :: Maybe Action -> ModifierType -> Int -> Int
applyActionCostModifier (Just action) (ActionCostOf (IsAction action') m) n
  | action == action' = n + m
applyActionCostModifier _ (ActionCostModifier m) n = n + m
applyActionCostModifier _ _ n = n

getCanAffordCost
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasCostPayment env
     , HasSet Trait env Source
     )
  => InvestigatorId
  -> Source
  -> Maybe Action
  -> Cost
  -> m Bool
getCanAffordCost iid source mAction = \case
  Free -> pure True
  UpTo{} -> pure True
  Costs xs -> and <$> traverse (getCanAffordCost iid source mAction) xs
  ExhaustCost target -> case target of
    AssetTarget aid -> do
      exhaustedAssetIds <- fmap unExhaustedAssetId <$> getSetList ()
      pure $ aid `notElem` exhaustedAssetIds
    _ -> error "Not handled"
  UseCost aid _uType n -> do
    uses <- unUsesCount <$> getCount aid
    pure $ uses >= n
  ActionCost n -> do
    modifiers <-
      map modifierType <$> getModifiersFor source (InvestigatorTarget iid) ()
    if ActionsAreFree `elem` modifiers
      then pure True
      else do
        let
          modifiedActionCost =
            foldr (applyActionCostModifier mAction) n modifiers
        traits <- getSetList @Trait source
        actionCount <- unActionRemainingCount
          <$> getCount (mAction, traits, iid)
        pure $ actionCount >= modifiedActionCost
  ClueCost n -> do
    spendableClues <- unSpendableClueCount <$> getCount iid
    pure $ spendableClues >= n
  GroupClueCost n Nothing -> do
    totalSpendableClues <- unSpendableClueCount <$> getCount ()
    pure $ totalSpendableClues >= n
  GroupClueCost n (Just locationMatcher) -> do
    mLocationId <- getId @(Maybe LocationId) locationMatcher
    case mLocationId of
      Just lid -> do
        iids <- getSetList @InvestigatorId lid
        totalSpendableClues <- sum
          <$> for iids ((unSpendableClueCount <$>) . getCount)
        pure $ totalSpendableClues >= n
      Nothing -> pure False
  ResourceCost n -> do
    resources <- unResourceCount <$> getCount iid
    pure $ resources >= n
  DiscardCost _ -> pure True -- TODO: Make better
  DiscardCardCost _ -> pure True -- TODO: Make better
  HorrorCost{} -> pure True -- TODO: Make better
  DamageCost{} -> pure True -- TODO: Make better
  SkillIconCost n skillTypes -> do
    handCards <- mapMaybe (preview _PlayerCard) <$> getHandOf iid
    let
      total = sum $ map
        (count (`member` insertSet SkillWild skillTypes) . pcSkills)
        handCards
    pure $ total >= n
  HandDiscardCost n mCardType traits skillTypes -> do
    cards <- mapMaybe (preview _PlayerCard) <$> getHandOf iid
    let
      cardTypeFilter = case mCardType of
        Nothing -> const True
        Just cardType -> (== cardType) . pcCardType
      traitFilter = if null traits
        then const True
        else not . null . intersect traits . pcTraits
      skillTypeFilter = if null skillTypes
        then const True
        else
          not
          . null
          . intersect (insertSet SkillWild skillTypes)
          . setFromList
          . pcSkills
    pure
      $ length
          (filter
            (and . sequence [traitFilter, cardTypeFilter, skillTypeFilter])
            cards
          )
      >= n

instance
  ( HasActions env ActionType
  , HasModifiersFor env ()
  , HasCostPayment env
  , HasSet Trait env Source
  , HasList UsedAbility env ()
  )
  => HasActions env () where
  getActions iid window _ = do
    actions' <- concat <$> traverse
      (getActions iid window)
      ([minBound .. maxBound] :: [ActionType])
    actions'' <- for actions' $ \case
      ActivateCardAbilityAction iid' ability -> do
        modifiers' <- getModifiersFor
          (InvestigatorSource iid)
          (sourceToTarget $ abilitySource ability)
          ()
        pure $ ActivateCardAbilityAction
          iid'
          (applyAbilityModifiers ability modifiers')
      other -> pure other -- TODO: dynamic abilities
    let
      fromForcedAction = \case
        Force msg -> Just msg
        _ -> Nothing
      forcedActions = mapMaybe fromForcedAction actions''
    if null forcedActions
      then do
        let
          canAffordAction = \case
            ActivateCardAbilityAction _ ability ->
              getCanAffordAbility iid ability
            _ -> pure True
        filterM canAffordAction actions''
      else pure forcedActions

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
getHasRecord = asks . hasRecord

getRecordSet
  :: (HasRecord env, MonadReader env m) => CampaignLogKey -> m [CardCode]
getRecordSet = asks . hasRecordSet

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
  -> m [Modifier]
getInvestigatorModifiers iid source =
  getModifiersFor source (InvestigatorTarget iid) ()

getXp :: (HasCount XPCount env (), MonadReader env m) => m Int
getXp = unXPCount <$> getCount ()

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
  sameLocation <- (== locationId) <$> getId @LocationId eid
  keywords <- getSet eid
  canAffordActions <- getCanAffordCost
    iid
    (EnemySource eid)
    (Just Action.Fight)
    (ActionCost 1)
  engagedInvestigators <- getSet eid
  pure
    $ canAffordActions
    && (Keyword.Aloof `notMember` keywords || iid `member` engagedInvestigators)
    && sameLocation

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
  enemyModifiers <-
    map modifierType
      <$> getModifiersFor (InvestigatorSource iid) (EnemyTarget eid) ()
  canAffordActions <- getCanAffordCost
    iid
    (EnemySource eid)
    (Just Action.Evade)
    (ActionCost 1)
  pure $ engaged && canAffordActions && CannotBeEvaded `notElem` enemyModifiers

getCanMoveTo
  :: ( MonadReader env m
     , HasCostPayment env
     , HasSet AccessibleLocationId env LocationId
     , HasSet Trait env Source
     , HasId LocationId env InvestigatorId
     , HasModifiersFor env ()
     )
  => LocationId
  -> InvestigatorId
  -> m Bool
getCanMoveTo lid iid = do
  locationId <- getId @LocationId iid
  modifiers' <-
    map modifierType
      <$> getModifiersFor (LocationSource lid) (InvestigatorTarget iid) ()
  locationModifiers' <-
    map modifierType
      <$> getModifiersFor (InvestigatorSource iid) (LocationTarget lid) ()
  accessibleLocations <- map unAccessibleLocationId <$> getSetList locationId
  canAffordActions <- getCanAffordCost
    iid
    (LocationSource lid)
    (Just Action.Move)
    (ActionCost 1)
  pure
    $ lid
    `elem` accessibleLocations
    && canAffordActions
    && lid
    /= locationId
    && CannotMove
    `notElem` modifiers'
    && Blocked
    `notElem` locationModifiers'

getCanInvestigate
  :: ( MonadReader env m
     , HasCostPayment env
     , HasId LocationId env InvestigatorId
     , HasSet Trait env Source
     , HasModifiersFor env ()
     )
  => LocationId
  -> InvestigatorId
  -> m Bool
getCanInvestigate lid iid = do
  locationId <- getId @LocationId iid
  canAffordActions <- getCanAffordCost
    iid
    (LocationSource lid)
    (Just Action.Investigate)
    (ActionCost 1)

  pure $ lid == locationId && canAffordActions

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
  SearchedCardTarget _ _ -> error "can not convert"
  EventTarget eid -> EventSource eid
  SkillTarget sid -> SkillSource sid
  SkillTestInitiatorTarget _ -> error "can not convert"
  TokenTarget tid -> TokenSource tid
  TokenFaceTarget _ -> error "Not convertable"
  DrawnTokenTarget dt -> DrawnTokenSource dt
  TestTarget -> TestSource mempty
  EncounterCardTarget _ -> error "can not convert"
  ResourceTarget -> ResourceSource
  InvestigationTarget{} -> error "not converted"

sourceToTarget :: Source -> Target
sourceToTarget = \case
  AssetSource aid -> AssetTarget aid
  EnemySource eid -> EnemyTarget eid
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
  DrawnTokenSource dt -> DrawnTokenTarget dt
  ProxySource _ source -> sourceToTarget source
  EffectSource eid -> EffectTarget eid
  ResourceSource -> ResourceTarget

addCampaignCardToDeckChoice
  :: InvestigatorId -> [InvestigatorId] -> CardCode -> Message
addCampaignCardToDeckChoice leadInvestigatorId investigatorIds cardCode =
  chooseOne
    leadInvestigatorId
    [ Label
      ("Add " <> tshow name <> " to a deck")
      [ chooseOne
          leadInvestigatorId
          [ TargetLabel
              (InvestigatorTarget iid)
              [AddCampaignCardToDeck iid cardCode]
          | iid <- investigatorIds
          ]
      ]
    , Label ("Do not add " <> tshow name <> " to any deck") []
    ]
  where name = lookupPlayerCardName cardCode
