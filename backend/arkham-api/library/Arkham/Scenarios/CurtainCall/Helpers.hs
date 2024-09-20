module Arkham.Scenarios.CurtainCall.Helpers where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Helpers
import Arkham.Helpers.Message
import Arkham.Id
import Arkham.Matcher hiding (Discarded)
import Arkham.Source
import Arkham.Target

moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
  :: (HasCallStack, HasGame m, HasQueue Message m) => m ()
moveTheManInThePalidMaskToLobbyInsteadOfDiscarding = do
  theManInThePallidMask <- getTheManInThePallidMask
  cancelEnemyDefeat theManInThePallidMask
  lobbyId <- selectJust $ location_ "Lobby"
  pushAll
    [HealAllDamage (toTarget theManInThePallidMask) GameSource, EnemyMove theManInThePallidMask lobbyId]

getTheManInThePallidMask :: (HasCallStack, HasGame m) => m EnemyId
getTheManInThePallidMask = selectJust (enemyIs Cards.theManInThePallidMask)
