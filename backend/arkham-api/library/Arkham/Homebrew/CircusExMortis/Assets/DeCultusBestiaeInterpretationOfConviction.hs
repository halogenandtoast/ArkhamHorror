module Arkham.Homebrew.CircusExMortis.Assets.DeCultusBestiaeInterpretationOfConviction (deCultusBestiaeInterpretationOfConviction) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.DestinyAndProphecy
import Arkham.Homebrew.CircusExMortis.Helpers (sealMoonTokenOnTarget)

newtype DeCultusBestiaeInterpretationOfConviction = DeCultusBestiaeInterpretationOfConviction AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deCultusBestiaeInterpretationOfConviction :: AssetCard DeCultusBestiaeInterpretationOfConviction
deCultusBestiaeInterpretationOfConviction = asset DeCultusBestiaeInterpretationOfConviction Cards.deCultusBestiaeInterpretationOfConviction

instance HasModifiersFor DeCultusBestiaeInterpretationOfConviction where
  getModifiersFor (DeCultusBestiaeInterpretationOfConviction a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities DeCultusBestiaeInterpretationOfConviction where
  getAbilities (DeCultusBestiaeInterpretationOfConviction a) = deCultusBestiaeAbilities 1 a

instance RunMessage DeCultusBestiaeInterpretationOfConviction where
  runMessage msg a@(DeCultusBestiaeInterpretationOfConviction attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sealMoonTokenOnTarget iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      deCultusBestiaeRelease iid attrs
      pure a
    _ -> DeCultusBestiaeInterpretationOfConviction <$> liftRunMessage msg attrs
