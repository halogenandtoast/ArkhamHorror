module Arkham.Types.Asset.Helpers where

import Arkham.Types.Classes
import Arkham.Types.Enemy
import Arkham.Types.Message
import ClassyPrelude
import Control.Monad.Extra (concatMapM)

hasFightActions
  :: (MonadReader env m, IsInvestigator investigator, HasList Enemy () env)
  => investigator
  -> m Bool
hasFightActions i = do
  enemyActions <- asks (concatMapM (getActions i) =<< getList @Enemy ())
  pure $ or [ True | FightEnemy{} <- enemyActions ]
