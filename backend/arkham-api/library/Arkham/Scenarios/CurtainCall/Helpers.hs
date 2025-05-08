module Arkham.Scenarios.CurtainCall.Helpers where

import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Message
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher hiding (Discarded)
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Control.Monad.Trans.Class

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "curtainCall" a

moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
  :: (HasCallStack, MonadTrans t, HasGame (t m), HasQueue Message m, HasQueue Message (t m)) => t m ()
moveTheManInThePalidMaskToLobbyInsteadOfDiscarding = do
  theManInThePallidMask <- getTheManInThePallidMask
  cancelEnemyDefeat theManInThePallidMask
  lobbyId <- selectJust $ location_ "Lobby"
  pushAll
    [HealAllDamage (toTarget theManInThePallidMask) GameSource, EnemyMove theManInThePallidMask lobbyId]

getTheManInThePallidMask :: (HasCallStack, HasGame m) => m EnemyId
getTheManInThePallidMask = selectJust (enemyIs Cards.theManInThePallidMask)
