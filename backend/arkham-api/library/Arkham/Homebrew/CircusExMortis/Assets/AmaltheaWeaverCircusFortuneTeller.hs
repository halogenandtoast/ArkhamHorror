module Arkham.Homebrew.CircusExMortis.Assets.AmaltheaWeaverCircusFortuneTeller (
  amaltheaWeaverCircusFortuneTeller,
) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.DestinyAndProphecy

newtype AmaltheaWeaverCircusFortuneTeller = AmaltheaWeaverCircusFortuneTeller AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amaltheaWeaverCircusFortuneTeller :: AssetCard AmaltheaWeaverCircusFortuneTeller
amaltheaWeaverCircusFortuneTeller =
  ally AmaltheaWeaverCircusFortuneTeller Cards.amaltheaWeaverCircusFortuneTeller (2, 2)

instance HasModifiersFor AmaltheaWeaverCircusFortuneTeller where
  getModifiersFor (AmaltheaWeaverCircusFortuneTeller a) =
    controllerGets a [SkillModifier #willpower 1]

instance HasAbilities AmaltheaWeaverCircusFortuneTeller where
  getAbilities (AmaltheaWeaverCircusFortuneTeller a) = amaltheaWeaverAbilities a

instance RunMessage AmaltheaWeaverCircusFortuneTeller where
  runMessage msg a@(AmaltheaWeaverCircusFortuneTeller attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      amaltheaWeaverBoost attrs
      pure a
    _ -> AmaltheaWeaverCircusFortuneTeller <$> liftRunMessage msg attrs
