module Arkham.Types.Enemy.Cards.FleshEater where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher

newtype FleshEater = FleshEater EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fleshEater :: EnemyCard FleshEater
fleshEater = enemy FleshEater Cards.fleshEater
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
