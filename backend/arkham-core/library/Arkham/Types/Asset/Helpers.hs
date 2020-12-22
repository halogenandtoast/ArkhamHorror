{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Arkham.Types.Asset.Helpers
  ( module Arkham.Types.Asset.Helpers
  , module X
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Game.Helpers as X
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Window

hasFightActions
  :: forall env m
   . (MonadIO m, MonadReader env m, HasActions env ActionType)
  => InvestigatorId
  -> Window
  -> m Bool
hasFightActions i NonFast = do
  enemyActions <- getActions i NonFast EnemyActionType
  pure $ or [ True | FightEnemy{} <- enemyActions ]
hasFightActions _ _ = pure False

hasInvestigateActions
  :: forall env m
   . (MonadIO m, MonadReader env m, HasActions env ActionType)
  => InvestigatorId
  -> Window
  -> m Bool
hasInvestigateActions i NonFast = do
  locationActions <- getActions i NonFast LocationActionType
  pure $ or [ True | Investigate{} <- locationActions ]
hasInvestigateActions _ _ = pure False
