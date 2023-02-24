module Arkham.Asset.Cards.HemisphericMap2
  ( hemisphericMap2
  , HemisphericMap2(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype HemisphericMap2 = HemisphericMap2 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor HemisphericMap2 where
  getModifiersFor (InvestigatorTarget iid) (HemisphericMap2 a)
    | controlledBy a iid = do
      connectedLocationCount <-
        selectCount $ ConnectedFrom $ locationWithInvestigator iid
      pure $ toModifiers a $ case connectedLocationCount of
        n | n >= 4 ->
          [SkillModifier SkillWillpower 2, SkillModifier SkillIntellect 2]
        n | n >= 2 ->
          [SkillModifier SkillWillpower 2, SkillModifier SkillIntellect 2]
        _ -> []
  getModifiersFor _ _ = pure []

hemisphericMap2 :: AssetCard HemisphericMap2
hemisphericMap2 = asset HemisphericMap2 Cards.hemisphericMap2

instance RunMessage HemisphericMap2 where
  runMessage msg (HemisphericMap2 attrs) =
    HemisphericMap2 <$> runMessage msg attrs
