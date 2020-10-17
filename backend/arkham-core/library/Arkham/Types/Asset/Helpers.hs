{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Arkham.Types.Asset.Helpers
  ( module Arkham.Types.Asset.Helpers
  , module X
  )
where

import Arkham.Types.Classes
import Arkham.Types.Window
import Arkham.Types.Message
import Arkham.Types.InvestigatorId
import ClassyPrelude
import Arkham.Types.Game.Helpers as X

hasFightActions
  :: forall env m
   . (MonadIO m, MonadReader env m, HasActions env (ActionType, env))
  => InvestigatorId
  -> Window
  -> m Bool
hasFightActions i NonFast = do
  enemyActions <- join $ asks (getActions i NonFast . (EnemyActionType, ))
  pure $ or [ True | FightEnemy{} <- enemyActions ]
hasFightActions _ _ = pure False

hasInvestigateActions
  :: forall env m
   . (MonadIO m, MonadReader env m, HasActions env (ActionType, env))
  => InvestigatorId
  -> Window
  -> m Bool
hasInvestigateActions i NonFast = do
  locationActions <- join $ asks (getActions i NonFast . (LocationActionType, ))
  pure $ or [ True | Investigate{} <- locationActions ]
hasInvestigateActions _ _ = pure False
