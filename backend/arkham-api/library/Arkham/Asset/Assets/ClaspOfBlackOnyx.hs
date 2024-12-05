module Arkham.Asset.Assets.ClaspOfBlackOnyx (claspOfBlackOnyx, ClaspOfBlackOnyx (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude

newtype ClaspOfBlackOnyx = ClaspOfBlackOnyx AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor ClaspOfBlackOnyx where
  getModifiersFor (ClaspOfBlackOnyx a) = case a.placement of
    StillInHand iid ->
      modified_
        a
        (InvestigatorHandTarget iid)
        [IncreaseCostOf (NotCard $ CardWithTitle "Clasp of Black Onyx") 1]
    _ -> pure mempty

claspOfBlackOnyx :: AssetCard ClaspOfBlackOnyx
claspOfBlackOnyx = asset ClaspOfBlackOnyx Cards.claspOfBlackOnyx

instance RunMessage ClaspOfBlackOnyx where
  runMessage msg (ClaspOfBlackOnyx attrs) =
    ClaspOfBlackOnyx <$> runMessage msg attrs
