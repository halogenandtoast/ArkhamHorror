module Arkham.Asset.Assets.CelaenoFragments where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype CelaenoFragments = CelaenoFragments AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

celaenoFragments :: AssetCard CelaenoFragments
celaenoFragments = asset CelaenoFragments Cards.celaenoFragments

instance HasModifiersFor CelaenoFragments where
  getModifiersFor (CelaenoFragments a) = case a.controller of
    Just iid -> do
      count' <- fieldMap InvestigatorHand length iid
      modified_ a iid
        $ [SkillModifier #intellect 1 | count' >= 5]
        <> [SkillModifier #willpower 1 | count' >= 10]
        <> [SkillModifier #intellect 1 | count' >= 15]
    Nothing -> pure mempty

instance RunMessage CelaenoFragments where
  runMessage msg (CelaenoFragments attrs) =
    CelaenoFragments <$> runMessage msg attrs
