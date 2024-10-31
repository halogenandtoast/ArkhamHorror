module Arkham.Asset.Assets.AlchemicalTransmutation (
  alchemicalTransmutation,
  AlchemicalTransmutation (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher

newtype AlchemicalTransmutation = AlchemicalTransmutation AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalTransmutation :: AssetCard AlchemicalTransmutation
alchemicalTransmutation = asset AlchemicalTransmutation Cards.alchemicalTransmutation

instance HasAbilities AlchemicalTransmutation where
  getAbilities (AlchemicalTransmutation a) =
    [ skillTestAbility
        $ restricted a 1 ControlsThis
        $ actionAbilityWithCost (exhaust a <> assetUseCost a Charge 1)
    ]

instance RunMessage AlchemicalTransmutation where
  runMessage msg a@(AlchemicalTransmutation attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      let tokens = oneOf [#skull, #cultist, #tablet, #elderthing, #autofail]
      onRevealChaosTokenEffect sid tokens attrs attrs do
        assignDamage iid (attrs.ability 1) 1

      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 1)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) (min 3 -> n) -> do
      push $ TakeResources iid n (attrs.ability 1) False
      pure a
    _ -> AlchemicalTransmutation <$> liftRunMessage msg attrs
