module Arkham.Enemy.Cards.PiperOfAzathoth
  ( piperOfAzathoth
  , PiperOfAzathoth(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype PiperOfAzathoth = PiperOfAzathoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

piperOfAzathoth :: EnemyCard PiperOfAzathoth
piperOfAzathoth = enemy PiperOfAzathoth Cards.piperOfAzathoth (5, Static 7, 2) (0, 2)

instance RunMessage PiperOfAzathoth where
  runMessage msg (PiperOfAzathoth attrs) =
    PiperOfAzathoth <$> runMessage msg attrs
