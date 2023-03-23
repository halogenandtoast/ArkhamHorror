module Arkham.Asset.Cards.OccultScraps
  ( occultScraps
  , OccultScraps(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Asset.Runner
import Arkham.Placement
import Arkham.SkillType

newtype OccultScraps = OccultScraps AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultScraps :: AssetCard OccultScraps
occultScraps =
  asset OccultScraps Cards.occultScraps

instance HasModifiersFor OccultScraps where
  getModifiersFor (InvestigatorTarget iid) (OccultScraps a) = do
    pure $ toModifiers a $ case assetPlacement a of
      InPlayArea iid' | iid == iid' -> [SkillModifier SkillWillpower (-1)]
      StillInHand iid' | iid == iid' -> [SkillModifier SkillWillpower (-2)]
      _ -> []
  getModifiersFor _ _ = pure []

instance RunMessage OccultScraps where
  runMessage msg (OccultScraps attrs) = OccultScraps <$> runMessage msg attrs
