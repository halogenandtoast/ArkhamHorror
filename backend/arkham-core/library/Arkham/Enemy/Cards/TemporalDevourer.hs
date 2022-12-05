module Arkham.Enemy.Cards.TemporalDevourer
  ( temporalDevourer
  , TemporalDevourer(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype TemporalDevourer = TemporalDevourer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

temporalDevourer :: EnemyCard TemporalDevourer
temporalDevourer =
  enemy TemporalDevourer Cards.temporalDevourer (4, Static 5, 4) (1, 1)

instance RunMessage TemporalDevourer where
  runMessage msg (TemporalDevourer attrs) =
    TemporalDevourer <$> runMessage msg attrs
