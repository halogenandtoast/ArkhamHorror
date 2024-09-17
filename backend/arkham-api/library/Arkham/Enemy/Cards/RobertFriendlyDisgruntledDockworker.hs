module Arkham.Enemy.Cards.RobertFriendlyDisgruntledDockworker
  ( robertFriendlyDisgruntledDockworker
  , RobertFriendlyDisgruntledDockworker(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype RobertFriendlyDisgruntledDockworker = RobertFriendlyDisgruntledDockworker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

robertFriendlyDisgruntledDockworker :: EnemyCard RobertFriendlyDisgruntledDockworker
robertFriendlyDisgruntledDockworker = enemy RobertFriendlyDisgruntledDockworker Cards.robertFriendlyDisgruntledDockworker (4, Static 4, 1) (2, 1)

instance RunMessage RobertFriendlyDisgruntledDockworker where
  runMessage msg (RobertFriendlyDisgruntledDockworker attrs) =
    RobertFriendlyDisgruntledDockworker <$> runMessage msg attrs
