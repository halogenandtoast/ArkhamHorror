module Arkham.Asset.Cards.ClaspOfBlackOnyx
  ( claspOfBlackOnyx
  , ClaspOfBlackOnyx(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Target

newtype ClaspOfBlackOnyx = ClaspOfBlackOnyx AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor env ClaspOfBlackOnyx where
  getModifiersFor _ (InvestigatorHandTarget _) (ClaspOfBlackOnyx attrs) =
    pure $ toModifiers
      attrs
      [IncreaseCostOf (NotCard $ CardWithTitle "Clasp of Black Onyx") 1]
  getModifiersFor _ _ _ = pure []

claspOfBlackOnyx :: AssetCard ClaspOfBlackOnyx
claspOfBlackOnyx = asset ClaspOfBlackOnyx Cards.claspOfBlackOnyx

instance AssetRunner env => RunMessage ClaspOfBlackOnyx where
  runMessage msg (ClaspOfBlackOnyx attrs) =
    ClaspOfBlackOnyx <$> runMessage msg attrs
