module Arkham.Scenarios.CurtainCall.Helpers where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher hiding (Discarded)
import Arkham.Message
import Arkham.Source
import Arkham.Target

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
