module Arkham.Enemy.Cards.StalkingManticore (stalkingManticore, StalkingManticore (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype StalkingManticore = StalkingManticore EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkingManticore :: EnemyCard StalkingManticore
stalkingManticore = enemy StalkingManticore Cards.stalkingManticore (4, PerPlayer 3, 2) (2, 1)

instance HasAbilities StalkingManticore where
  getAbilities (StalkingManticore a) =
    extend
      a
      [ mkAbility a 1
          $ forced
          $ SkillTestResult #when You (WhileEvadingAnEnemy $ be a) (SuccessResult AnyValue)
      ]

instance RunMessage StalkingManticore where
  runMessage msg e@(StalkingManticore attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- we may double up the DoNotExhaustEvaded but it should not matter
      push $ phaseModifiers (attrs.ability 1) attrs [DoNotExhaustEvaded, CannotEngage iid]
      pure e
    _ -> StalkingManticore <$> runMessage msg attrs
