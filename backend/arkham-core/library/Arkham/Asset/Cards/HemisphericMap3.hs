module Arkham.Asset.Cards.HemisphericMap3 (
  hemisphericMap3,
  HemisphericMap3 (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype HemisphericMap3 = HemisphericMap3 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

instance HasModifiersFor HemisphericMap3 where
  getModifiersFor (InvestigatorTarget iid) (HemisphericMap3 a)
    | controlledBy a iid = do
        connectedLocationCount <-
          selectCount $ ConnectedFrom $ locationWithInvestigator iid
        pure $ toModifiers a $ case connectedLocationCount of
          n
            | n >= 4 ->
                [SkillModifier SkillWillpower 2, SkillModifier SkillIntellect 2]
          n
            | n >= 2 ->
                [SkillModifier SkillWillpower 2, SkillModifier SkillIntellect 2]
          _ -> []
  getModifiersFor _ _ = pure []

hemisphericMap3 :: AssetCard HemisphericMap3
hemisphericMap3 = asset HemisphericMap3 Cards.hemisphericMap3

instance RunMessage HemisphericMap3 where
  runMessage msg (HemisphericMap3 attrs) =
    HemisphericMap3 <$> runMessage msg attrs
