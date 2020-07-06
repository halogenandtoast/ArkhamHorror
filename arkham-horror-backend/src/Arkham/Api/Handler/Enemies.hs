module Arkham.Api.Handler.Enemies
  ( postApiV1ArkhamGameSelectEnemyR
  )
where

import Arkham.Types
import Arkham.Types.Enemy
import Arkham.Types.Game
import Arkham.Types.GameState
import Arkham.Types.Player
import Arkham.Util
import Data.Aeson
import Data.Aeson.Casing
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.UUID
import Import
import Lens.Micro
import Safe (fromJustNote)

newtype SelectEnemyPost = SelectEnemyPost { sepEnemyId :: UUID }
  deriving stock (Generic, Show)

instance FromJSON SelectEnemyPost where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

-- , _enemyHealthDamage :: Int
-- , _enemySanityDamage :: Int
-- , _enemyFinishedAttacking :: Bool
postApiV1ArkhamGameSelectEnemyR :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameSelectEnemyR gameId = do
  game <- runDB $ get404 gameId
  enemyId <- sepEnemyId <$> requireCheckJsonBody
  let
    enemy' =
      fromJustNote "Unknown enemy" $ HashMap.lookup enemyId (game ^. enemies)
    players' = HashMap.map (performAttackIfEngaged enemy') (game ^. players)
    ArkhamGameStateStepResolveEnemiesStep ArkhamResolveEnemiesStep {..} =
      game ^. gameStateStep
  runDB
    $ updateGame gameId
    $ game
    & enemies
    . at enemyId
    ?~ enemy' { _enemyFinishedAttacking = True }
    & players
    .~ players'
    & gameStateStep
    .~ ArkhamGameStateStepResolveEnemiesStep
         (ArkhamResolveEnemiesStep $ HashSet.delete enemyId aresEnemyIds)

-- TODO: massive
performAttackIfEngaged :: ArkhamEnemy -> ArkhamPlayer -> ArkhamPlayer
performAttackIfEngaged enemy' player' =
  if _enemyId enemy' `elem` _enemies player'
    then
      player'
      & healthDamage
      +~ _enemyHealthDamage enemy'
      & sanityDamage
      +~ _enemySanityDamage enemy'
    else player'
