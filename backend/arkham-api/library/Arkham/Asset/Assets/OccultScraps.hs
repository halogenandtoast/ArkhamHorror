module Arkham.Asset.Assets.OccultScraps (
  occultScraps,
  OccultScraps (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Placement

newtype OccultScraps = OccultScraps AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultScraps :: AssetCard OccultScraps
occultScraps = asset OccultScraps Cards.occultScraps

instance HasModifiersFor OccultScraps where
  getModifiersFor (OccultScraps a) = case a.placement of
    InPlayArea iid -> modified_ a iid [SkillModifier #willpower (-1)]
    StillInHand iid -> modified_ a iid [SkillModifier #willpower (-2)]
    _ -> pure mempty

instance RunMessage OccultScraps where
  runMessage msg (OccultScraps attrs) = OccultScraps <$> runMessage msg attrs
