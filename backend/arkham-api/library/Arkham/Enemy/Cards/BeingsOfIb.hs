module Arkham.Enemy.Cards.BeingsOfIb (beingsOfIb) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Ruins))

newtype BeingsOfIb = BeingsOfIb EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

beingsOfIb :: EnemyCard BeingsOfIb
beingsOfIb =
  enemyWith BeingsOfIb Cards.beingsOfIb (4, Static 1, 4) (0, 1) \a ->
    a
      & (preyL .~ OnlyPrey (Prey $ at_ $ LocationWithTrait Ruins))
      & (attacksL .~ at_ (locationWithEnemy a))

instance RunMessage BeingsOfIb where
  runMessage msg (BeingsOfIb attrs) = BeingsOfIb <$> runMessage msg attrs
