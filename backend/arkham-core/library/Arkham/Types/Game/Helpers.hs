module Arkham.Types.Game.Helpers where

import Arkham.Import

import Arkham.Types.Action (Action)
import qualified Arkham.Types.Action as Action
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
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanFight _ iid = getHasActionsRemaining iid (Just Action.Fight) mempty

-- TODO: canEngage enemy a@Attrs {..} = canDo Action.Engage a && getId () enemy `notElem` investigatorEngagedEnemies
getCanEngage
  :: ( MonadReader env m
     , HasCount ActionRemainingCount (InvestigatorId, Maybe Action, [Trait]) env
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanEngage _ iid = getHasActionsRemaining iid (Just Action.Engage) mempty

-- TODO: canEvade enemy a@Attrs {..} = canDo Action.Evade a && getId () enemy `elem` investigatorEngagedEnemies
getCanEvade
  :: ( MonadReader env m
     , HasCount ActionRemainingCount (InvestigatorId, Maybe Action, [Trait]) env
     )
  => EnemyId
  -> InvestigatorId
  -> m Bool
getCanEvade _ iid = getHasActionsRemaining iid (Just Action.Evade) mempty

-- TODO: canMoveTo location a@Attrs {..} = canDo Action.Move a && getId () location `elem` investigatorConnectedLocations
getCanMoveTo
  :: ( MonadReader env m
     , HasCount ActionRemainingCount (InvestigatorId, Maybe Action, [Trait]) env
     )
  => LocationId
  -> InvestigatorId
  -> m Bool
getCanMoveTo _ iid = getHasActionsRemaining iid (Just Action.Move) mempty

-- TODO: canInvestigate location a@Attrs {..} = canDo Action.Investigate a && getId () location == investigatorLocation
getCanInvestigate
  :: ( MonadReader env m
     , HasCount ActionRemainingCount (InvestigatorId, Maybe Action, [Trait]) env
     )
  => LocationId
  -> InvestigatorId
  -> m Bool
getCanInvestigate _ iid =
  getHasActionsRemaining iid (Just Action.Investigate) mempty

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
