module Arkham.Homebrew.CircusExMortis.Assets.DeCultusBestiaeForgottenWorkOfApuleius (deCultusBestiaeForgottenWorkOfApuleius) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.DestinyAndProphecy
import Arkham.Homebrew.CircusExMortis.Helpers (sealMoonTokenOnTarget)

newtype DeCultusBestiaeForgottenWorkOfApuleius = DeCultusBestiaeForgottenWorkOfApuleius AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deCultusBestiaeForgottenWorkOfApuleius :: AssetCard DeCultusBestiaeForgottenWorkOfApuleius
deCultusBestiaeForgottenWorkOfApuleius = asset DeCultusBestiaeForgottenWorkOfApuleius Cards.deCultusBestiaeForgottenWorkOfApuleius

instance HasModifiersFor DeCultusBestiaeForgottenWorkOfApuleius where
  getModifiersFor (DeCultusBestiaeForgottenWorkOfApuleius a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities DeCultusBestiaeForgottenWorkOfApuleius where
  getAbilities (DeCultusBestiaeForgottenWorkOfApuleius a) = deCultusBestiaeAbilities 0 a

instance RunMessage DeCultusBestiaeForgottenWorkOfApuleius where
  runMessage msg a@(DeCultusBestiaeForgottenWorkOfApuleius attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sealMoonTokenOnTarget iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      deCultusBestiaeRelease iid attrs
      pure a
    _ -> DeCultusBestiaeForgottenWorkOfApuleius <$> liftRunMessage msg attrs
