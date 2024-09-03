module Arkham.Asset.Cards.RabbitsFoot where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype RabbitsFoot = RabbitsFoot AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rabbitsFoot :: AssetCard RabbitsFoot
rabbitsFoot = asset RabbitsFoot Cards.rabbitsFoot

instance HasAbilities RabbitsFoot where
  getAbilities (RabbitsFoot a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (SkillTestResult #after You AnySkillTest (FailureResult AnyValue)) (exhaust a)
    ]

instance RunMessage RabbitsFoot where
  runMessage msg a@(RabbitsFoot attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    _ -> RabbitsFoot <$> runMessage msg attrs
