module Arkham.Scenarios.CurtainCall.Helpers where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher hiding ( Discarded )
import Arkham.Message
import Arkham.Source
import Arkham.Target

moveTheManInThePalidMaskToLobbyInsteadOfDiscarding :: GameT ()
moveTheManInThePalidMaskToLobbyInsteadOfDiscarding = do
  theManInThePallidMask <- getTheManInThePallidMask
  lobbyId <- fromJustNote "Lobby must be in play"
    <$> selectOne (LocationWithTitle "Lobby")
  popMessageMatching_ \case
    RemovedFromPlay (EnemySource eid) -> eid == theManInThePallidMask
    _ -> False
  replaceMessageMatching
    \case
      Discarded (EnemyTarget eid) _ _ -> eid == theManInThePallidMask
      _ -> False
    (const [EnemyMove theManInThePallidMask lobbyId])

getTheManInThePallidMask :: HasGame m => m EnemyId
getTheManInThePallidMask =
  fromJustNote "the man in the pallid mask must still be in play"
    <$> selectOne (enemyIs Cards.theManInThePallidMask)
