module Arkham.Homebrew.CircusExMortis.Assets.DeCultusBestiaeProphecyOfTheEternal (deCultusBestiaeProphecyOfTheEternal) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.DestinyAndProphecy
import Arkham.Homebrew.CircusExMortis.Helpers (sealMoonTokenOnTarget)

newtype DeCultusBestiaeProphecyOfTheEternal = DeCultusBestiaeProphecyOfTheEternal AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deCultusBestiaeProphecyOfTheEternal :: AssetCard DeCultusBestiaeProphecyOfTheEternal
deCultusBestiaeProphecyOfTheEternal = asset DeCultusBestiaeProphecyOfTheEternal Cards.deCultusBestiaeProphecyOfTheEternal

instance HasModifiersFor DeCultusBestiaeProphecyOfTheEternal where
  getModifiersFor (DeCultusBestiaeProphecyOfTheEternal a) = controllerGets a [SkillModifier #intellect 1, SkillModifier #agility 1]

instance HasAbilities DeCultusBestiaeProphecyOfTheEternal where
  getAbilities (DeCultusBestiaeProphecyOfTheEternal a) = deCultusBestiaeAbilities 1 a

instance RunMessage DeCultusBestiaeProphecyOfTheEternal where
  runMessage msg a@(DeCultusBestiaeProphecyOfTheEternal attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sealMoonTokenOnTarget iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      deCultusBestiaeRelease iid attrs
      pure a
    _ -> DeCultusBestiaeProphecyOfTheEternal <$> liftRunMessage msg attrs
