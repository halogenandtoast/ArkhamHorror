module Arkham.Enemy.Cards.BeingsOfIb (beingsOfIb, BeingsOfIb (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Ruins))

newtype BeingsOfIb = BeingsOfIb EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

beingsOfIb :: EnemyCard BeingsOfIb
beingsOfIb =
  enemyWith
    BeingsOfIb
    Cards.beingsOfIb
    (4, Static 1, 4)
    (0, 1)
    $ \a ->
      a
        & preyL
        .~ OnlyPrey (InvestigatorAt $ LocationWithTrait Ruins)
        & attacksL
        .~ InvestigatorAt (locationWithEnemy (toId a))

instance RunMessage BeingsOfIb where
  runMessage msg (BeingsOfIb attrs) =
    BeingsOfIb <$> runMessage msg attrs
