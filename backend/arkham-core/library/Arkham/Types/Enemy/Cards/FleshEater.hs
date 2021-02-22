module Arkham.Types.Enemy.Cards.FleshEater where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher

newtype FleshEater = FleshEater EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fleshEater :: EnemyId -> FleshEater
fleshEater uuid =
  FleshEater
    $ baseAttrs uuid "01118"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 2)
    . (fightL .~ 4)
    . (healthL .~ Static 4)
    . (evadeL .~ 1)
    . (spawnAtL ?~ LocationWithTitle "Attic")

instance HasModifiersFor env FleshEater where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env FleshEater where
  getActions i window (FleshEater attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env FleshEater where
  runMessage msg (FleshEater attrs) = FleshEater <$> runMessage msg attrs
