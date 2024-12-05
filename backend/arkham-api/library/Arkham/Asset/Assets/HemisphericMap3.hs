module Arkham.Asset.Assets.HemisphericMap3 (
  hemisphericMap3,
  HemisphericMap3 (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype HemisphericMap3 = HemisphericMap3 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor HemisphericMap3 where
  getModifiersFor (HemisphericMap3 a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      connectedLocationCount <- selectCount $ ConnectedFrom $ locationWithInvestigator iid
      modified_ a iid $ case connectedLocationCount of
        n | n >= 4 -> [SkillModifier #willpower 2, SkillModifier #intellect 2]
        n | n >= 2 -> [SkillModifier #willpower 1, SkillModifier #intellect 1]
        _ -> []

hemisphericMap3 :: AssetCard HemisphericMap3
hemisphericMap3 = asset HemisphericMap3 Cards.hemisphericMap3

instance RunMessage HemisphericMap3 where
  runMessage msg (HemisphericMap3 attrs) =
    HemisphericMap3 <$> runMessage msg attrs
