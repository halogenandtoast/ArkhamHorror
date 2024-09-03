module Arkham.Asset.Cards.Cornered2 (cornered2, Cornered2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Cornered2 = Cornered2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cornered2 :: AssetCard Cornered2
cornered2 = asset Cornered2 Cards.cornered2

instance HasAbilities Cornered2 where
  getAbilities (Cornered2 a) =
    [ playerLimit PerTestOrAbility
        $ controlledAbility a 1 (DuringSkillTest AnySkillTest)
        $ FastAbility
        $ HandDiscardCost 1 #any
    ]

instance RunMessage Cornered2 where
  runMessage msg a@(Cornered2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid attrs iid $ AnySkillValue 2
      pure a
    _ -> Cornered2 <$> runMessage msg attrs
