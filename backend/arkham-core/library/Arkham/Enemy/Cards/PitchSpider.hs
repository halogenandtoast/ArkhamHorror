module Arkham.Enemy.Cards.PitchSpider
  ( pitchSpider
  , PitchSpider(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype PitchSpider = PitchSpider EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pitchSpider :: EnemyCard PitchSpider
pitchSpider = enemy PitchSpider Cards.pitchSpider (2, Static 1, 4) (1, 1)

instance RunMessage PitchSpider where
  runMessage msg (PitchSpider attrs) =
    PitchSpider <$> runMessage msg attrs
