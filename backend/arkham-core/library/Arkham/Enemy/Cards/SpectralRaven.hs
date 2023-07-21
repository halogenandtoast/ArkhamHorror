module Arkham.Enemy.Cards.SpectralRaven (
  spectralRaven,
  SpectralRaven (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype SpectralRaven = SpectralRaven EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

spectralRaven :: EnemyCard SpectralRaven
spectralRaven = enemy SpectralRaven Cards.spectralRaven (2, Static 2, 2) (1, 1)

instance RunMessage SpectralRaven where
  runMessage msg (SpectralRaven attrs) =
    SpectralRaven <$> runMessage msg attrs
