module Arkham.Asset.Cards.ArcaneStudies4 (
  arcaneStudies4,
  ArcaneStudies4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype ArcaneStudies4 = ArcaneStudies4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneStudies4 :: AssetCard ArcaneStudies4
arcaneStudies4 = asset ArcaneStudies4 Cards.arcaneStudies4

instance HasAbilities ArcaneStudies4 where
  getAbilities (ArcaneStudies4 a) =
    [ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ OrCost [ResourceCost 1, UseCost (AssetWithId $ toId a) Resource 1]
    ]

instance RunMessage ArcaneStudies4 where
  runMessage msg a@(ArcaneStudies4 attrs) = case msg of
    Do BeginRound -> pure . ArcaneStudies4 $ attrs & usesL . ix Resource %~ max 2
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Choose Willpower" [skillTestModifier attrs iid (SkillModifier #willpower 1)]
          , Label "Choose Intellect" [skillTestModifier attrs iid (SkillModifier #intellect 1)]
          ]
      pure a
    _ -> ArcaneStudies4 <$> runMessage msg attrs
