module Arkham.Enemy.Cards.PiperOfAzathoth (piperOfAzathoth, PiperOfAzathoth (..)) where

import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype PiperOfAzathoth = PiperOfAzathoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, AsId)

piperOfAzathoth :: EnemyCard PiperOfAzathoth
piperOfAzathoth = enemy PiperOfAzathoth Cards.piperOfAzathoth (5, Static 7, 2) (0, 2)

instance HasAbilities PiperOfAzathoth where
  getAbilities (PiperOfAzathoth a) = extend a [mkAbility a 1 $ forced $ PhaseEnds #when #enemy]

instance RunMessage PiperOfAzathoth where
  runMessage msg e@(PiperOfAzathoth attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- selectList $ at_ (locationWithEnemy e) <> not_ (investigatorEngagedWith e)
      pushAll $ map (toMessage . enemyAttack attrs (toAbilitySource attrs 1)) investigators
      pure e
    _ -> PiperOfAzathoth <$> runMessage msg attrs
