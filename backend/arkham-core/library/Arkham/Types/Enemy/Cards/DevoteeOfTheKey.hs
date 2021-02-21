module Arkham.Types.Enemy.Cards.DevoteeOfTheKey
  ( devoteeOfTheKey
  , DevoteeOfTheKey(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.EnemyId
import Arkham.Types.GameValue

newtype DevoteeOfTheKey = DevoteeOfTheKey EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devoteeOfTheKey :: EnemyId -> DevoteeOfTheKey
devoteeOfTheKey uuid =
  DevoteeOfTheKey
    $ baseAttrs uuid "02294"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 3)
    . (healthL .~ Static 3)
    . (evadeL .~ 3)

instance HasModifiersFor env DevoteeOfTheKey where
  getModifiersFor = noModifiersFor

instance EnemyAttrsHasActions env => HasActions env DevoteeOfTheKey where
  getActions i window (DevoteeOfTheKey attrs) = getActions i window attrs

instance EnemyAttrsRunMessage env => RunMessage env DevoteeOfTheKey where
  runMessage msg (DevoteeOfTheKey attrs) =
    DevoteeOfTheKey <$> runMessage msg attrs
