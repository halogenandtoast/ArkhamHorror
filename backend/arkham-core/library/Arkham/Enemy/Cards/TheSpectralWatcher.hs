module Arkham.Enemy.Cards.TheSpectralWatcher
  ( theSpectralWatcher
  , TheSpectralWatcher(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype TheSpectralWatcher = TheSpectralWatcher EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theSpectralWatcher :: EnemyCard TheSpectralWatcher
theSpectralWatcher = enemy TheSpectralWatcher Cards.theSpectralWatcher (3, Static 5, 3) (1, 1)

instance RunMessage TheSpectralWatcher where
  runMessage msg (TheSpectralWatcher attrs) =
    TheSpectralWatcher <$> runMessage msg attrs
