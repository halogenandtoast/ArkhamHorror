module Arkham.Homebrew.CircusExMortis.Assets.AmaltheaWeaverAspirantOfCourage (
  amaltheaWeaverAspirantOfCourage,
) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.DestinyAndProphecy

newtype AmaltheaWeaverAspirantOfCourage = AmaltheaWeaverAspirantOfCourage AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amaltheaWeaverAspirantOfCourage :: AssetCard AmaltheaWeaverAspirantOfCourage
amaltheaWeaverAspirantOfCourage =
  ally AmaltheaWeaverAspirantOfCourage Cards.amaltheaWeaverAspirantOfCourage (3, 2)

instance HasModifiersFor AmaltheaWeaverAspirantOfCourage where
  getModifiersFor (AmaltheaWeaverAspirantOfCourage a) =
    controllerGets a [SkillModifier #willpower 1]

instance HasAbilities AmaltheaWeaverAspirantOfCourage where
  getAbilities (AmaltheaWeaverAspirantOfCourage a) = amaltheaWeaverAbilities a

instance RunMessage AmaltheaWeaverAspirantOfCourage where
  runMessage msg a@(AmaltheaWeaverAspirantOfCourage attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      amaltheaWeaverBoost attrs
      amaltheaWeaverRider attrs msg
      pure a
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      amaltheaWeaverRelease attrs 1
      pure a
    _ -> AmaltheaWeaverAspirantOfCourage <$> liftRunMessage msg attrs
