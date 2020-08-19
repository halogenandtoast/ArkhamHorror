module Arkham.Types.Asset.Helpers where

import Arkham.Types.Classes
import Arkham.Types.Enemy
import Arkham.Types.Message
import Arkham.Types.FastWindow (FastWindow)
import ClassyPrelude
import Control.Monad.Extra (concatMapM)

hasFightActions
  :: (MonadReader env m, IsInvestigator investigator, HasList Enemy () env)
  => investigator
  -> FastWindow
  -> m Bool
hasFightActions i window = do
  enemyActions <- asks (concatMapM (getActions i window) =<< getList @Enemy ())
  pure $ or [ True | FightEnemy{} <- enemyActions ]
