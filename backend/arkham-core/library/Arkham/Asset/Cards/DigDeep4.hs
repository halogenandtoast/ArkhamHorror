module Arkham.Asset.Cards.DigDeep4 (digDeep4, DigDeep4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype DigDeep4 = DigDeep4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

digDeep4 :: AssetCard DigDeep4
digDeep4 = asset DigDeep4 Cards.digDeep4

instance HasAbilities DigDeep4 where
  getAbilities (DigDeep4 a) =
    [ controlledAbility a 1 (DuringSkillTest AnySkillTest)
        $ FastAbility
        $ OrCost [ResourceCost 1, assetUseCost a Resource 1]
    ]

instance RunMessage DigDeep4 where
  runMessage msg a@(DigDeep4 attrs) = case msg of
    Do BeginRound -> pure . DigDeep4 $ attrs & tokensL . ix Resource %~ max 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      let source = attrs.ability 1
      push
        $ chooseOne
          player
          [ Label "Choose Willpower" [skillTestModifier source iid (SkillModifier #willpower 1)]
          , Label "Choose Agility" [skillTestModifier source iid (SkillModifier #agility 1)]
          ]
      pure a
    _ -> DigDeep4 <$> runMessage msg attrs
