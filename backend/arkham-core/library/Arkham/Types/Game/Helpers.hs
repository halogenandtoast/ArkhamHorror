module Arkham.Types.Game.Helpers where

import Arkham.Import

import Arkham.Types.Action (Action)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Keyword
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Trait (Trait)

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

getHasActionsRemaining
  :: ( MonadReader env m
     , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
     )
  => InvestigatorId
  -> Maybe Action
  -> [Trait]
  -> m Bool
getHasActionsRemaining iid maction traits =
  (> 0) . unActionRemainingCount <$> getCount (maction, traits, iid)


-- TODO: canFight _ a@Attrs {..} = canDo Action.Fight a
getCanFight
  :: ( MonadReader env m
     , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
     , HasSet InvestigatorId env EnemyId
     , HasSet Keyword env EnemyId
     , HasId LocationId env InvestigatorId
     , HasId LocationId env EnemyId
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanFight eid iid = do
  locationId <- getId @LocationId iid
  sameLocation <- (== locationId) <$> getId @LocationId eid
  keywords <- getSet eid
  hasActionsRemaining <- getHasActionsRemaining iid (Just Action.Fight) mempty
  engagedInvestigators <- getSet eid
  pure
    $ hasActionsRemaining
    && (Keyword.Aloof `notMember` keywords || iid `member` engagedInvestigators)
    && sameLocation

getCanEngage
  :: ( MonadReader env m
     , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
     , HasSet InvestigatorId env EnemyId
     , HasId LocationId env InvestigatorId
     , HasId LocationId env EnemyId
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanEngage eid iid = do
  locationId <- getId @LocationId iid
  sameLocation <- (== locationId) <$> getId @LocationId eid
  notEngaged <- notElem iid <$> getSet eid
  hasActionsRemaining <- getHasActionsRemaining iid (Just Action.Engage) mempty
  pure $ notEngaged && hasActionsRemaining && sameLocation

getCanEvade
  :: ( MonadReader env m
     , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
     , HasSet InvestigatorId env EnemyId
     , HasModifiersFor env ()
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanEvade eid iid = do
  engaged <- elem iid <$> getSet eid
  enemyModifiers <- getModifiersFor
    (InvestigatorSource iid)
    (EnemyTarget eid)
    ()
  hasActionsRemaining <- getHasActionsRemaining iid (Just Action.Evade) mempty
  pure
    $ engaged
    && hasActionsRemaining
    && CannotBeEvaded
    `notElem` enemyModifiers

getCanMoveTo
  :: ( MonadReader env m
     , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
     , HasSet AccessibleLocationId env LocationId
     , HasId LocationId env InvestigatorId
     , HasModifiersFor env ()
     )
  => LocationId
  -> InvestigatorId
  -> m Bool
getCanMoveTo lid iid = do
  locationId <- getId @LocationId iid
  modifiers' <- getModifiersFor (LocationSource lid) (InvestigatorTarget iid) ()
  locationModifiers' <- getModifiersFor
    (InvestigatorSource iid)
    (LocationTarget lid)
    ()
  accessibleLocations <- map unAccessibleLocationId <$> getSetList locationId
  hasActionsRemaining <- getHasActionsRemaining iid (Just Action.Move) mempty
  pure
    $ lid
    `elem` accessibleLocations
    && hasActionsRemaining
    && lid
    /= locationId
    && CannotMove
    `notElem` modifiers'
    && Blocked
    `notElem` locationModifiers'

getCanInvestigate
  :: ( MonadReader env m
     , HasCount ActionRemainingCount env (Maybe Action, [Trait], InvestigatorId)
     , HasId LocationId env InvestigatorId
     , HasSet Trait env LocationId
     , HasModifiersFor env ()
     )
  => LocationId
  -> InvestigatorId
  -> m Bool
getCanInvestigate lid iid = do
  locationId <- getId @LocationId iid
  traits <- getSetList @Trait lid
  actionsRemaining <- unActionRemainingCount
    <$> getCount (Just Action.Investigate, traits, iid)
  modifiers' <- getModifiersFor (InvestigatorSource iid) (LocationTarget lid) ()

  let investigateCost = foldr applyModifier 1 modifiers'

  pure $ lid == locationId && actionsRemaining >= investigateCost
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
