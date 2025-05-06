module Arkham.Asset.Assets.TheNecronomiconOlausWormiusTranslation (
  theNecronomiconOlausWormiusTranslation,
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers

newtype TheNecronomiconOlausWormiusTranslation = TheNecronomiconOlausWormiusTranslation AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNecronomiconOlausWormiusTranslation
  :: AssetCard TheNecronomiconOlausWormiusTranslation
theNecronomiconOlausWormiusTranslation =
  asset TheNecronomiconOlausWormiusTranslation Cards.theNecronomiconOlausWormiusTranslation

instance HasAbilities TheNecronomiconOlausWormiusTranslation where
  getAbilities (TheNecronomiconOlausWormiusTranslation a) = [restricted a 1 ControlsThis actionAbility]

instance HasModifiersFor TheNecronomiconOlausWormiusTranslation where
  getModifiersFor (TheNecronomiconOlausWormiusTranslation a) = controllerGets a [SkillModifier #intellect 1]

instance RunMessage TheNecronomiconOlausWormiusTranslation where
  runMessage msg a@(TheNecronomiconOlausWormiusTranslation attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 2
      pure a
    _ -> TheNecronomiconOlausWormiusTranslation <$> liftRunMessage msg attrs
