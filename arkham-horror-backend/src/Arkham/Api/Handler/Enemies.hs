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
import Data.UUID
import Import
import Lens.Micro
import Safe (fromJustNote)

newtype SelectEnemyPost = SelectEnemyPost { sepEnemyId :: UUID }
  deriving stock (Generic, Show)

instance FromJSON SelectEnemyPost where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

-- brittany-disable-next-binding
postApiV1ArkhamGameSelectEnemyR :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameSelectEnemyR gameId = do
  game <- runDB $ get404 gameId
  enemyId <- sepEnemyId <$> requireCheckJsonBody
  let
    enemy' = findEnemy game enemyId
    players' = HashMap.map (performAttackIfEngaged enemy') (game ^. players)
    ArkhamGameStateStepResolveEnemiesStep ArkhamResolveEnemiesStep {..} =
      game ^. gameStateStep
  runDB
    $ updateGame gameId
    $ game & enemies . ix enemyId . finishedAttacking .~ True
           & players .~ players'

-- TODO: massive enemies
-- brittany-disable-next-binding
performAttackIfEngaged :: ArkhamEnemy -> ArkhamPlayer -> ArkhamPlayer
performAttackIfEngaged ArkhamEnemy {..} player' =
  if _enemyId `elem` _enemies player'
    then
      player' & healthDamage +~ _enemyHealthDamage
              & sanityDamage +~ _enemySanityDamage
    else player'

findEnemy :: ArkhamGame -> UUID -> ArkhamEnemy
findEnemy game enemyId =
  fromJustNote "Unknown enemy" $ game ^. enemies . at enemyId
