module Arkham.Asset.Cards.HardKnocks4 (
  hardKnocks4,
  HardKnocks4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype HardKnocks4 = HardKnocks4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hardKnocks4 :: AssetCard HardKnocks4
hardKnocks4 = asset HardKnocks4 Cards.hardKnocks4

instance HasAbilities HardKnocks4 where
  getAbilities (HardKnocks4 a) =
    [ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ OrCost [ResourceCost 1, UseCost (AssetWithId $ toId a) Resource 1]
    ]

instance RunMessage HardKnocks4 where
  runMessage msg a@(HardKnocks4 attrs) = case msg of
    Do BeginRound -> pure . HardKnocks4 $ attrs & usesL .~ Uses Resource 2
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Choose Combat" [skillTestModifier attrs iid (SkillModifier #combat 1)]
          , Label "Choose Agility" [skillTestModifier attrs iid (SkillModifier #agility 1)]
          ]
      pure a
    _ -> HardKnocks4 <$> runMessage msg attrs
