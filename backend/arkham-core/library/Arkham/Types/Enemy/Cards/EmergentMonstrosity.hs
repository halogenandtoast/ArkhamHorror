module Arkham.Types.Enemy.Cards.EmergentMonstrosity
  ( EmergentMonstrosity(..)
  , emergentMonstrosity
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype EmergentMonstrosity = EmergentMonstrosity EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergentMonstrosity :: EnemyId -> EmergentMonstrosity
emergentMonstrosity uuid =
  EmergentMonstrosity
    $ baseAttrs uuid "02183"
    $ (healthDamageL .~ 2)
    . (sanityDamageL .~ 2)
    . (fightL .~ 4)
    . (healthL .~ Static 5)
    . (evadeL .~ 4)

instance HasModifiersFor env EmergentMonstrosity where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env EmergentMonstrosity where
  getActions i window (EmergentMonstrosity attrs) = getActions i window attrs

instance EnemyRunner env => RunMessage env EmergentMonstrosity where
  runMessage msg (EmergentMonstrosity attrs@EnemyAttrs {..}) = case msg of

    InvestigatorDrawEnemy iid _ eid | eid == enemyId -> do
      lid <- getId @LocationId iid
      spawnLocation <- fromMaybe lid <$> getId (RightOf, lid)
      unshiftMessage (EnemySpawn (Just iid) spawnLocation enemyId)
      pure . EmergentMonstrosity $ attrs & exhaustedL .~ True
    _ -> EmergentMonstrosity <$> runMessage msg attrs
