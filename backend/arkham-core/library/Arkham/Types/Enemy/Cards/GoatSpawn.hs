module Arkham.Types.Enemy.Cards.GoatSpawn
  ( goatSpawn
  , GoatSpawn(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message
import Arkham.Types.Source

newtype GoatSpawn = GoatSpawn EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

goatSpawn :: EnemyCard GoatSpawn
goatSpawn = enemy GoatSpawn Cards.goatSpawn (3, Static 3, 2) (1, 0)

instance HasModifiersFor env GoatSpawn where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env GoatSpawn where
  getActions i window (GoatSpawn attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GoatSpawn where
  runMessage msg (GoatSpawn attrs@EnemyAttrs {..}) = case msg of
    EnemyDefeated eid _ _ _ _ _ | eid == enemyId -> do
      investigatorIds <- getSetList enemyLocation
      pushAll
        [ InvestigatorAssignDamage iid (EnemySource eid) DamageAny 0 1
        | iid <- investigatorIds
        ]
      GoatSpawn <$> runMessage msg attrs
    _ -> GoatSpawn <$> runMessage msg attrs
