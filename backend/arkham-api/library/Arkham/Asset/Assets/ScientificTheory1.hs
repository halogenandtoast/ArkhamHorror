module Arkham.Asset.Assets.ScientificTheory1 (scientificTheory1, ScientificTheory1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype ScientificTheory1 = ScientificTheory1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scientificTheory1 :: AssetCard ScientificTheory1
scientificTheory1 =
  assetWith ScientificTheory1 Cards.scientificTheory1 (sanityL ?~ 1)

instance HasAbilities ScientificTheory1 where
  getAbilities (ScientificTheory1 x) =
    [ withTooltip "{fast} Spend 1 resource: You get +1 {intellect} for this skill test."
        $ wantsSkillTest (YourSkillTest #intellect)
        $ controlledAbility x 1 DuringAnySkillTest (FastAbility $ ResourceCost 1)
    , withTooltip "{fast} Spend 1 resource: You get +1 {combat} for this skill test."
        $ wantsSkillTest (YourSkillTest #combat)
        $ controlledAbility x 2 DuringAnySkillTest (FastAbility $ ResourceCost 1)
    ]

instance HasModifiersFor ScientificTheory1 where
  getModifiersFor (ScientificTheory1 a) = modifySelf a [NonDirectHorrorMustBeAssignToThisFirst]

instance RunMessage ScientificTheory1 where
  runMessage msg a@(ScientificTheory1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        pushM $ skillTestModifier sid attrs iid (SkillModifier #intellect 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        pushM $ skillTestModifier sid attrs iid (SkillModifier #combat 1)
      pure a
    _ -> ScientificTheory1 <$> runMessage msg attrs
