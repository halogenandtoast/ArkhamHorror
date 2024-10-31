module Arkham.Asset.Assets.AlchemicalTransmutation2 (
  alchemicalTransmutation2,
  AlchemicalTransmutation2 (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher

newtype AlchemicalTransmutation2 = AlchemicalTransmutation2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalTransmutation2 :: AssetCard AlchemicalTransmutation2
alchemicalTransmutation2 = asset AlchemicalTransmutation2 Cards.alchemicalTransmutation2

instance HasAbilities AlchemicalTransmutation2 where
  getAbilities (AlchemicalTransmutation2 a) =
    [ skillTestAbility
        $ restricted a 1 ControlsThis
        $ actionAbilityWithCost (exhaust a <> assetUseCost a Charge 1)
    ]

instance RunMessage AlchemicalTransmutation2 where
  runMessage msg a@(AlchemicalTransmutation2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      let tokens = oneOf [#skull, #cultist, #tablet, #elderthing, #autofail]
      onRevealChaosTokenEffect sid tokens attrs attrs do
        assignDamage iid (attrs.ability 1) 1
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 0)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) (min 4 -> n) -> do
      push $ TakeResources iid n (attrs.ability 1) False
      pure a
    _ -> AlchemicalTransmutation2 <$> liftRunMessage msg attrs
