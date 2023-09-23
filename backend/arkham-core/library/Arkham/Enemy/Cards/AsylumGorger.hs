module Arkham.Enemy.Cards.AsylumGorger (
  asylumGorger,
  AsylumGorger (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype AsylumGorger = AsylumGorger EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor AsylumGorger where
  getModifiersFor target (AsylumGorger a) | isTarget a target = do
    pure
      $ toModifiers a
      $ CannotMakeAttacksOfOpportunity
      : [CannotAttack | enemyMovedFromHunterKeyword a]
  getModifiersFor _ _ = pure []

asylumGorger :: EnemyCard AsylumGorger
asylumGorger = enemy AsylumGorger Cards.asylumGorger (4, Static 5, 4) (3, 3)

instance RunMessage AsylumGorger where
  runMessage msg (AsylumGorger attrs) = AsylumGorger <$> runMessage msg attrs
