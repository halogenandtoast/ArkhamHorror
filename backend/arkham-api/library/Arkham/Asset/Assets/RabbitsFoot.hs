module Arkham.Asset.Assets.RabbitsFoot (rabbitsFoot) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher

newtype RabbitsFoot = RabbitsFoot AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rabbitsFoot :: AssetCard RabbitsFoot
rabbitsFoot = asset RabbitsFoot Cards.rabbitsFoot

instance HasAbilities RabbitsFoot where
  getAbilities (RabbitsFoot a) =
    [ restricted a 1 ControlsThis
        $ triggered (SkillTestResult #after You AnySkillTest #failure) (exhaust a)
    ]

instance RunMessage RabbitsFoot where
  runMessage msg a@(RabbitsFoot attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure a
    _ -> RabbitsFoot <$> liftRunMessage msg attrs
