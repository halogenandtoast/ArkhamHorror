module Arkham.Asset.Assets.Grounded1 (grounded1, Grounded1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait

newtype Grounded1 = Grounded1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grounded1 :: AssetCard Grounded1
grounded1 = assetWith Grounded1 Cards.grounded1 (sanityL ?~ 1)

instance HasAbilities Grounded1 where
  getAbilities (Grounded1 x) =
    [ wantsSkillTest (YourSkillTest AnySkillTest)
        $ controlledAbility x 1 (DuringSkillTest $ SkillTestSourceMatches $ SourceWithTrait Spell)
        $ FastAbility
        $ ResourceCost 1
    ]

instance HasModifiersFor Grounded1 where
  getModifiersFor (Grounded1 attrs) = modifySelf attrs [NonDirectHorrorMustBeAssignToThisFirst]

instance RunMessage Grounded1 where
  runMessage msg a@(Grounded1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        pushM $ skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
      pure a
    _ -> Grounded1 <$> runMessage msg attrs
