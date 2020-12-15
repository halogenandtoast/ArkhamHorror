module Arkham.Types.Game.Helpers where

import Arkham.Import

import Arkham.Types.Action (Action)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Keyword
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Trait (Trait)

withBaseActions
  :: (MonadIO m, HasActions env a, MonadReader env m)
  => InvestigatorId
  -> Window
  -> a
  -> m [Message]
  -> m [Message]
withBaseActions iid window a f = (<>) <$> getActions iid window a <*> f

getIsUnused
  :: (HasList UsedAbility env (), MonadReader env m)
  => InvestigatorId
  -> Ability
  -> m Bool
getIsUnused iid ability = notElem ability' . map unUsedAbility <$> getList ()
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
     , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
     , HasCount SpendableClueCount env InvestigatorId
     , HasSet InvestigatorId env EnemyId
     , HasSet Keyword env EnemyId
     , HasSet Trait env EnemyId
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
  traits <- getSet eid
  canAffordActions <- getCanAffordCost
    iid
    (EnemySource eid)
    (ActionCost 1 (Just Action.Fight) traits)
  engagedInvestigators <- getSet eid
  pure
    $ canAffordActions
    && (Keyword.Aloof `notMember` keywords || iid `member` engagedInvestigators)
    && sameLocation

getCanEngage
  :: ( MonadReader env m
     , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
     , HasCount SpendableClueCount env InvestigatorId
     , HasSet InvestigatorId env EnemyId
     , HasSet Trait env EnemyId
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
  traits <- getSet eid
  canAffordActions <- getCanAffordCost
    iid
    (EnemySource eid)
    (ActionCost 1 (Just Action.Engage) traits)
  pure $ notEngaged && canAffordActions && sameLocation

getCanEvade
  :: ( MonadReader env m
     , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
     , HasCount SpendableClueCount env InvestigatorId
     , HasSet InvestigatorId env EnemyId
     , HasSet Trait env EnemyId
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
  traits <- getSet eid
  canAffordActions <- getCanAffordCost
    iid
    (EnemySource eid)
    (ActionCost 1 (Just Action.Evade) traits)
  pure $ engaged && canAffordActions && CannotBeEvaded `notElem` enemyModifiers

getCanMoveTo
  :: ( MonadReader env m
     , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
     , HasCount SpendableClueCount env InvestigatorId
     , HasSet AccessibleLocationId env LocationId
     , HasSet Trait env LocationId
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
  traits <- getSet lid
  canAffordActions <- getCanAffordCost
    iid
    (LocationSource lid)
    (ActionCost 1 (Just Action.Move) traits)
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
     , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
     , HasCount SpendableClueCount env InvestigatorId
     , HasId LocationId env InvestigatorId
     , HasSet Trait env LocationId
     , HasModifiersFor env ()
     )
  => LocationId
  -> InvestigatorId
  -> m Bool
getCanInvestigate lid iid = do
  locationId <- getId @LocationId iid
  traits <- getSet @Trait lid
  modifiers' <-
    map modifierType
      <$> getModifiersFor (InvestigatorSource iid) (LocationTarget lid) ()

  let investigateCost = foldr applyModifier 1 modifiers'

  canAffordActions <- getCanAffordCost
    iid
    (LocationSource lid)
    (ActionCost investigateCost (Just Action.Investigate) traits)

  pure $ lid == locationId && canAffordActions
 where
  applyModifier (ActionCostOf (IsAction Action.Investigate) m) n =
    max 0 (n + m)
  applyModifier _ n = n

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

getCanAffordCost
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
     , HasCount SpendableClueCount env InvestigatorId
     )
  => InvestigatorId
  -> Source
  -> Cost
  -> m Bool
getCanAffordCost iid source = \case
  ActionCost n mAction traits -> do
    modifiers <-
      map modifierType <$> getModifiersFor source (InvestigatorTarget iid) ()
    if ActionsAreFree `elem` modifiers
      then pure True
      else do
        actionCount <- unActionRemainingCount
          <$> getCount (mAction, setToList traits, iid)
        pure $ actionCount >= n
  ClueCost n -> do
    spendableClues <- unSpendableClueCount <$> getCount iid
    pure $ spendableClues >= n

toModifier :: Entity a => a -> ModifierType -> Modifier
toModifier = Modifier . toSource

toModifiers :: Entity a => a -> [ModifierType] -> [Modifier]
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
  TestTarget -> TestSource
  EncounterCardTarget _ -> error "can not convert"

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
  TestSource -> TestTarget
  DrawnTokenSource dt -> DrawnTokenTarget dt
  ProxySource _ _ -> error "not implemented"
  EffectSource eid -> EffectTarget eid
