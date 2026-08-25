module Arkham.Homebrew.CircusExMortis.Assets.AmaltheaWeaverAspirantOfWisdom (
  amaltheaWeaverAspirantOfWisdom,
) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.DestinyAndProphecy

newtype AmaltheaWeaverAspirantOfWisdom = AmaltheaWeaverAspirantOfWisdom AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amaltheaWeaverAspirantOfWisdom :: AssetCard AmaltheaWeaverAspirantOfWisdom
amaltheaWeaverAspirantOfWisdom =
  ally AmaltheaWeaverAspirantOfWisdom Cards.amaltheaWeaverAspirantOfWisdom (2, 3)

instance HasModifiersFor AmaltheaWeaverAspirantOfWisdom where
  getModifiersFor (AmaltheaWeaverAspirantOfWisdom a) =
    controllerGets a [SkillModifier #willpower 1]

instance HasAbilities AmaltheaWeaverAspirantOfWisdom where
  getAbilities (AmaltheaWeaverAspirantOfWisdom a) = amaltheaWeaverAbilities a

instance RunMessage AmaltheaWeaverAspirantOfWisdom where
  runMessage msg a@(AmaltheaWeaverAspirantOfWisdom attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      amaltheaWeaverBoost attrs
      amaltheaWeaverRider attrs msg
      pure a
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      amaltheaWeaverChooseRecipient attrs \iid -> drawCards iid (attrs.ability 1) 1
      pure a
    _ -> AmaltheaWeaverAspirantOfWisdom <$> liftRunMessage msg attrs
