module Arkham.Types.Game.Helpers where

import Arkham.Import

import Arkham.Types.Action (Action)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Keyword
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Trait (Trait)

getIsUnused
  :: (HasList UsedAbility () env, MonadReader env m)
  => InvestigatorId
  -> Ability
  -> m Bool
getIsUnused iid ability =
  asks $ notElem ability' . map unUsedAbility . getList ()
  where ability' = (iid, ability)

getGroupIsUnused
  :: (MonadReader env m, HasList UsedAbility () env) => Ability -> m Bool
getGroupIsUnused ability =
  asks $ notElem ability . map (snd . unUsedAbility) . getList ()

getInvestigatorModifiers
  :: (MonadReader env m, HasModifiersFor env env, MonadIO m)
  => InvestigatorId
  -> Source
  -> m [Modifier]
getInvestigatorModifiers iid source =
  ask >>= getModifiersFor source (InvestigatorTarget iid)

getXp :: (HasCount XPCount () env, MonadReader env m) => m Int
getXp = asks $ unXPCount . getCount ()

getLeadInvestigatorId
  :: (HasId LeadInvestigatorId () env, MonadReader env m) => m InvestigatorId
getLeadInvestigatorId = asks $ unLeadInvestigatorId . getId ()

getInvestigatorIds
  :: (HasSet InvestigatorId () env, MonadReader env m) => m [InvestigatorId]
getInvestigatorIds = asks $ setToList . getSet ()

getPlayerCount :: (HasCount PlayerCount () env, MonadReader env m) => m Int
getPlayerCount = asks $ unPlayerCount . getCount ()

getPlayerCountValue
  :: (HasCount PlayerCount () env, MonadReader env m) => GameValue Int -> m Int
getPlayerCountValue gameValue = fromGameValue gameValue <$> getPlayerCount

getLocationSet
  :: (HasSet LocationId () env, MonadReader env m) => m (HashSet LocationId)
getLocationSet = asks $ getSet ()

getSpendableClueCount
  :: (MonadReader env m, HasCount SpendableClueCount InvestigatorId env)
  => [InvestigatorId]
  -> m Int
getSpendableClueCount investigatorIds =
  sum <$> for investigatorIds (asks . (unSpendableClueCount .) . getCount)

getHasActionsRemaining
  :: ( MonadReader env m
     , HasCount ActionRemainingCount (InvestigatorId, Maybe Action, [Trait]) env
     )
  => InvestigatorId
  -> Maybe Action
  -> [Trait]
  -> m Bool
getHasActionsRemaining iid maction traits =
  asks $ (> 0) . unActionRemainingCount . getCount (iid, maction, traits)


-- TODO: canFight _ a@Attrs {..} = canDo Action.Fight a
getCanFight
  :: ( MonadReader env m
     , HasCount ActionRemainingCount (InvestigatorId, Maybe Action, [Trait]) env
     , HasSet InvestigatorId EnemyId env
     , HasSet Keyword EnemyId env
     , HasId LocationId InvestigatorId env
     , HasId LocationId EnemyId env
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanFight eid iid = do
  locationId <- asks $ getId @LocationId iid
  sameLocation <- asks $ (== locationId) . getId @LocationId eid
  keywords <- asks $ getSet eid
  hasActionsRemaining <- getHasActionsRemaining iid (Just Action.Fight) mempty
  engagedInvestigators <- asks $ getSet eid
  pure
    $ hasActionsRemaining
    && (Keyword.Aloof `notMember` keywords || iid `member` engagedInvestigators)
    && sameLocation

getCanEngage
  :: ( MonadReader env m
     , HasCount ActionRemainingCount (InvestigatorId, Maybe Action, [Trait]) env
     , HasSet InvestigatorId EnemyId env
     , HasId LocationId InvestigatorId env
     , HasId LocationId EnemyId env
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanEngage eid iid = do
  locationId <- asks $ getId @LocationId iid
  sameLocation <- asks $ (== locationId) . getId @LocationId eid
  notEngaged <- asks $ notElem iid . getSet eid
  hasActionsRemaining <- getHasActionsRemaining iid (Just Action.Engage) mempty
  pure $ notEngaged && hasActionsRemaining && sameLocation

getCanEvade
  :: ( MonadReader env m
     , HasCount ActionRemainingCount (InvestigatorId, Maybe Action, [Trait]) env
     , HasSet InvestigatorId EnemyId env
     , HasModifiersFor env env
     , MonadIO m
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanEvade eid iid = do
  engaged <- asks $ elem iid . getSet eid
  enemyModifiers <-
    getModifiersFor (InvestigatorSource iid) (EnemyTarget eid) =<< ask
  hasActionsRemaining <- getHasActionsRemaining iid (Just Action.Evade) mempty
  pure
    $ engaged
    && hasActionsRemaining
    && CannotBeEvaded
    `notElem` enemyModifiers

getCanMoveTo
  :: ( MonadReader env m
     , HasCount ActionRemainingCount (InvestigatorId, Maybe Action, [Trait]) env
     , HasSet AccessibleLocationId LocationId env
     , HasId LocationId InvestigatorId env
     , HasModifiersFor env env
     , MonadIO m
     )
  => LocationId
  -> InvestigatorId
  -> m Bool
getCanMoveTo lid iid = do
  locationId <- asks $ getId @LocationId iid
  modifiers' <-
    getModifiersFor (LocationSource lid) (InvestigatorTarget iid) =<< ask
  accessibleLocations <-
    asks $ map unAccessibleLocationId . setToList . getSet locationId
  hasActionsRemaining <- getHasActionsRemaining iid (Just Action.Move) mempty
  pure
    $ lid
    `elem` accessibleLocations
    && hasActionsRemaining
    && lid
    /= locationId
    && CannotMove
    `notElem` modifiers'

getCanInvestigate
  :: ( MonadReader env m
     , HasCount ActionRemainingCount (InvestigatorId, Maybe Action, [Trait]) env
     , HasId LocationId InvestigatorId env
     )
  => LocationId
  -> InvestigatorId
  -> m Bool
getCanInvestigate lid iid = do
  locationId <- asks $ getId @LocationId iid
  hasActionsRemaining <- getHasActionsRemaining
    iid
    (Just Action.Investigate)
    mempty
  pure $ lid == locationId && hasActionsRemaining

getResourceCount
  :: (MonadReader env m, HasCount ResourceCount InvestigatorId env)
  => InvestigatorId
  -> m Int
getResourceCount iid = asks $ unResourceCount . getCount iid

getDiscardOf
  :: (MonadReader env m, HasList DiscardedPlayerCard InvestigatorId env)
  => InvestigatorId
  -> m [PlayerCard]
getDiscardOf iid = asks $ map unDiscardedPlayerCard . getList iid

getHandOf
  :: (MonadReader env m, HasList HandCard InvestigatorId env)
  => InvestigatorId
  -> m [Card]
getHandOf iid = asks $ map unHandCard . getList iid

getInPlayOf
  :: (MonadReader env m, HasList InPlayCard InvestigatorId env)
  => InvestigatorId
  -> m [Card]
getInPlayOf iid = asks $ map unInPlayCard . getList iid

getCardCount
  :: (MonadReader env m, HasCount CardCount InvestigatorId env)
  => InvestigatorId
  -> m Int
getCardCount iid = asks $ unCardCount . getCount iid
