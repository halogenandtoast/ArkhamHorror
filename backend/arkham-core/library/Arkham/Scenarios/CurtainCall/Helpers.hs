module Arkham.Scenarios.CurtainCall.Helpers where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher hiding (Discarded)
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
  :: ( MonadIO m
     , MonadReader env m
     , HasQueue env
     , Query LocationMatcher env
     , Query EnemyMatcher env
     , HasId LocationId env EnemyId
     )
  => m ()
moveTheManInThePalidMaskToLobbyInsteadOfDiscarding = do
  theManInThePallidMask <- getTheManInThePallidMask
  lid <- getId theManInThePallidMask
  lobbyId <- fromJustNote "Lobby must be in play"
    <$> selectOne (LocationWithTitle "Lobby")
  popMessageMatching_ \case
    RemovedFromPlay (EnemySource eid) -> eid == theManInThePallidMask
    _ -> False
  replaceMessageMatching
    \case
      Discarded (EnemyTarget eid) _ -> eid == theManInThePallidMask
      _ -> False
    (const [EnemyMove theManInThePallidMask lid lobbyId])

getTheManInThePallidMask
  :: (MonadReader env m, Query EnemyMatcher env) => m EnemyId
getTheManInThePallidMask =
  fromJustNote "the man in the pallid mask must still be in play"
    <$> selectOne (enemyIs Cards.theManInThePallidMask)
