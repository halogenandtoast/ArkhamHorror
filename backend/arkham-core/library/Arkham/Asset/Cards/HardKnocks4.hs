module Arkham.Asset.Cards.HardKnocks4 (hardKnocks4, HardKnocks4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype HardKnocks4 = HardKnocks4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hardKnocks4 :: AssetCard HardKnocks4
hardKnocks4 = asset HardKnocks4 Cards.hardKnocks4

instance HasAbilities HardKnocks4 where
  getAbilities (HardKnocks4 a) =
    [ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ OrCost [ResourceCost 1, assetUseCost a Resource 1]
    ]

instance RunMessage HardKnocks4 where
  runMessage msg a@(HardKnocks4 attrs) = case msg of
    Do BeginRound -> pure . HardKnocks4 $ attrs & tokensL . ix Resource %~ max 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        player <- getPlayer iid
        let source = attrs.ability 1
        push
          $ chooseOne
            player
            [ Label "Choose Combat" [skillTestModifier sid source iid (SkillModifier #combat 1)]
            , Label "Choose Agility" [skillTestModifier sid source iid (SkillModifier #agility 1)]
            ]
      pure a
    _ -> HardKnocks4 <$> runMessage msg attrs
