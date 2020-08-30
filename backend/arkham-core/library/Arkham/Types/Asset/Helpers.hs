{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Arkham.Types.Asset.Helpers where

import Arkham.Types.Classes
import Arkham.Types.Window
import Arkham.Types.Message
import ClassyPrelude

hasFightActions
  :: forall investigator env m
   . ( MonadIO m
     , MonadReader env m
     , HasActions env investigator (ActionType, env)
     )
  => investigator
  -> Window
  -> m Bool
hasFightActions i NonFast = do
  enemyActions <- join $ asks (getActions i NonFast . (EnemyActionType, ))
  pure $ or [ True | FightEnemy{} <- enemyActions ]
hasFightActions _ _ = pure False

hasInvestigateActions
  :: forall investigator env m
   . ( MonadIO m
     , MonadReader env m
     , HasActions env investigator (ActionType, env)
     )
  => investigator
  -> Window
  -> m Bool
hasInvestigateActions i NonFast = do
  locationActions <- join $ asks (getActions i NonFast . (LocationActionType, ))
  pure $ or [ True | Investigate{} <- locationActions ]
hasInvestigateActions _ _ = pure False
