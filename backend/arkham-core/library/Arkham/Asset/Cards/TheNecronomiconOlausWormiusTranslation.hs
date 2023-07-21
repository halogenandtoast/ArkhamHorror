module Arkham.Asset.Cards.TheNecronomiconOlausWormiusTranslation (
  theNecronomiconOlausWormiusTranslation,
  TheNecronomiconOlausWormiusTranslation (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.SkillType

newtype TheNecronomiconOlausWormiusTranslation = TheNecronomiconOlausWormiusTranslation AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNecronomiconOlausWormiusTranslation
  :: AssetCard TheNecronomiconOlausWormiusTranslation
theNecronomiconOlausWormiusTranslation =
  asset
    TheNecronomiconOlausWormiusTranslation
    Cards.theNecronomiconOlausWormiusTranslation

instance HasAbilities TheNecronomiconOlausWormiusTranslation where
  getAbilities (TheNecronomiconOlausWormiusTranslation a) =
    [restrictedAbility a 1 ControlsThis $ ActionAbility Nothing $ ActionCost 1]

instance HasModifiersFor TheNecronomiconOlausWormiusTranslation where
  getModifiersFor (InvestigatorTarget iid) (TheNecronomiconOlausWormiusTranslation a) =
    pure $ toModifiers a [SkillModifier SkillIntellect 1 | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance RunMessage TheNecronomiconOlausWormiusTranslation where
  runMessage msg a@(TheNecronomiconOlausWormiusTranslation attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ TakeResources iid 2 (toAbilitySource attrs 1) False
      pure a
    _ -> TheNecronomiconOlausWormiusTranslation <$> runMessage msg attrs
