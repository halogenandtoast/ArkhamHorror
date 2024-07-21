module Arkham.Asset.Cards.Hyperawareness4 (hyperawareness4, Hyperawareness4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype Hyperawareness4 = Hyperawareness4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hyperawareness4 :: AssetCard Hyperawareness4
hyperawareness4 = asset Hyperawareness4 Cards.hyperawareness4

instance HasAbilities Hyperawareness4 where
  getAbilities (Hyperawareness4 a) =
    [ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ OrCost [ResourceCost 1, assetUseCost a Resource 1]
    ]

instance RunMessage Hyperawareness4 where
  runMessage msg a@(Hyperawareness4 attrs) = case msg of
    Do BeginRound -> pure . Hyperawareness4 $ attrs & tokensL . ix Resource %~ max 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        player <- getPlayer iid
        let source = attrs.ability 1
        push
          $ chooseOne
            player
            [ Label "Choose Intellect" [skillTestModifier sid source iid (SkillModifier #intellect 1)]
            , Label "Choose Agility" [skillTestModifier sid source iid (SkillModifier #agility 1)]
            ]
      pure a
    _ -> Hyperawareness4 <$> runMessage msg attrs
