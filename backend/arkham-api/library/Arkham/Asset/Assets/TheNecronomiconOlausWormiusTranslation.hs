module Arkham.Asset.Assets.TheNecronomiconOlausWormiusTranslation (
  theNecronomiconOlausWormiusTranslation,
  TheNecronomiconOlausWormiusTranslation (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype TheNecronomiconOlausWormiusTranslation = TheNecronomiconOlausWormiusTranslation AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNecronomiconOlausWormiusTranslation
  :: AssetCard TheNecronomiconOlausWormiusTranslation
theNecronomiconOlausWormiusTranslation =
  asset
    TheNecronomiconOlausWormiusTranslation
    Cards.theNecronomiconOlausWormiusTranslation

instance HasAbilities TheNecronomiconOlausWormiusTranslation where
  getAbilities (TheNecronomiconOlausWormiusTranslation a) =
    [restrictedAbility a 1 ControlsThis $ ActionAbility [] $ ActionCost 1]

instance HasModifiersFor TheNecronomiconOlausWormiusTranslation where
  getModifiersFor (TheNecronomiconOlausWormiusTranslation a) = controllerGets a [SkillModifier #intellect 1]

instance RunMessage TheNecronomiconOlausWormiusTranslation where
  runMessage msg a@(TheNecronomiconOlausWormiusTranslation attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ TakeResources iid 2 (toAbilitySource attrs 1) False
      pure a
    _ -> TheNecronomiconOlausWormiusTranslation <$> runMessage msg attrs
