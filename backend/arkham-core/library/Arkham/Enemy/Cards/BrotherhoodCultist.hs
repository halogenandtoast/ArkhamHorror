module Arkham.Enemy.Cards.BrotherhoodCultist (brotherhoodCultist, BrotherhoodCultist (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner hiding (EnemyEvade, EnemyFight)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype BrotherhoodCultist = BrotherhoodCultist EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

brotherhoodCultist :: EnemyCard BrotherhoodCultist
brotherhoodCultist = enemy BrotherhoodCultist Cards.brotherhoodCultist (2, Static 3, 2) (0, 1)

instance HasModifiersFor BrotherhoodCultist where
  getModifiersFor target (BrotherhoodCultist a) | a `is` target = do
    doom <- field EnemyDoom (toId a)
    pure . toModifiers a $ guard (doom > 0) *> [EnemyFight doom, EnemyEvade doom]
  getModifiersFor _ _ = pure []

instance HasAbilities BrotherhoodCultist where
  getAbilities (BrotherhoodCultist a) =
    extend a [mkAbility a 1 $ forced $ EnemyAttacked #when You AnySource $ be a]

instance RunMessage BrotherhoodCultist where
  runMessage msg e@(BrotherhoodCultist attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ placeDoom attrs attrs 1
      pure e
    _ -> BrotherhoodCultist <$> runMessage msg attrs
