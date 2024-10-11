module Arkham.Enemy.Cards.StalkingManticore (stalkingManticore, StalkingManticore (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype StalkingManticore = StalkingManticore EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkingManticore :: EnemyCard StalkingManticore
stalkingManticore = enemy StalkingManticore Cards.stalkingManticore (4, PerPlayer 3, 2) (2, 1)

instance HasAbilities StalkingManticore where
  getAbilities (StalkingManticore a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ SkillTestResult #when You (WhileEvadingAnEnemy $ be a) (SuccessResult AnyValue)

instance RunMessage StalkingManticore where
  runMessage msg e@(StalkingManticore attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- we may double up the DoNotExhaustEvaded but it should not matter
      phaseModifiers (attrs.ability 1) attrs [DoNotExhaustEvaded, CannotEngage iid]
      pure e
    _ -> StalkingManticore <$> liftRunMessage msg attrs
