module Arkham.Homebrew.CircusExMortis.Assets.DeCultusBestiaeProphecyOfTheBeyond (deCultusBestiaeProphecyOfTheBeyond) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.DestinyAndProphecy
import Arkham.Homebrew.CircusExMortis.Helpers (sealMoonTokenOnTarget)

newtype DeCultusBestiaeProphecyOfTheBeyond = DeCultusBestiaeProphecyOfTheBeyond AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deCultusBestiaeProphecyOfTheBeyond :: AssetCard DeCultusBestiaeProphecyOfTheBeyond
deCultusBestiaeProphecyOfTheBeyond = asset DeCultusBestiaeProphecyOfTheBeyond Cards.deCultusBestiaeProphecyOfTheBeyond

instance HasModifiersFor DeCultusBestiaeProphecyOfTheBeyond where
  getModifiersFor (DeCultusBestiaeProphecyOfTheBeyond a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities DeCultusBestiaeProphecyOfTheBeyond where
  getAbilities (DeCultusBestiaeProphecyOfTheBeyond a) = deCultusBestiaeAbilities 2 a

instance RunMessage DeCultusBestiaeProphecyOfTheBeyond where
  runMessage msg a@(DeCultusBestiaeProphecyOfTheBeyond attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sealMoonTokenOnTarget iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      deCultusBestiaeRelease iid attrs
      pure a
    _ -> DeCultusBestiaeProphecyOfTheBeyond <$> liftRunMessage msg attrs
