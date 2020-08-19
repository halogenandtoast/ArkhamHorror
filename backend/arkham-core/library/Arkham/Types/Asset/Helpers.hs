{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Arkham.Types.Asset.Helpers where

import Arkham.Types.Classes
import Arkham.Types.FastWindow
import Arkham.Types.Message
import ClassyPrelude

hasFightActions
  :: forall investigator env m
   . (MonadReader env m, HasActions env investigator (ActionType, env))
  => investigator
  -> FastWindow
  -> m Bool
hasFightActions i window@(DuringTurn You) = do
  enemyActions <- asks (join $ getActions i window . (EnemyActionType, ))
  pure $ or [ True | FightEnemy{} <- enemyActions ]
hasFightActions _ _ = pure False

hasInvestigateActions
  :: forall investigator env m
   . (MonadReader env m, HasActions env investigator (ActionType, env))
  => investigator
  -> FastWindow
  -> m Bool
hasInvestigateActions i window@(DuringTurn You) = do
  locationActions <- asks (join $ getActions i window . (LocationActionType, ))
  pure $ or [ True | Investigate{} <- locationActions ]
hasInvestigateActions _ _ = pure False
