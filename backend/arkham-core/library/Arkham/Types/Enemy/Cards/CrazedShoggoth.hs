module Arkham.Types.Enemy.Cards.CrazedShoggoth
  ( CrazedShoggoth(..)
  , crazedShoggoth
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.EnemyId
import Arkham.Types.GameValue

newtype CrazedShoggoth = CrazedShoggoth EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crazedShoggoth :: EnemyId -> CrazedShoggoth
crazedShoggoth uuid =
  CrazedShoggoth
    $ baseAttrs uuid "02295"
    $ (healthDamageL .~ 2)
    . (sanityDamageL .~ 2)
    . (fightL .~ 3)
    . (healthL .~ Static 6)
    . (evadeL .~ 4)

deriving newtype instance EnemyAttrsHasActions env => HasActions env CrazedShoggoth

instance HasModifiersFor env CrazedShoggoth where
  getModifiersFor = noModifiersFor

instance EnemyAttrsRunMessage env => RunMessage env CrazedShoggoth where
  runMessage msg (CrazedShoggoth attrs) =
    CrazedShoggoth <$> runMessage msg attrs
