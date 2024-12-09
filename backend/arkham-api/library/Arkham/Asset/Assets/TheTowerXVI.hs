module Arkham.Asset.Assets.TheTowerXVI (theTowerXVI, TheTowerXVI (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude

newtype TheTowerXVI = TheTowerXVI AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTowerXVI :: AssetCard TheTowerXVI
theTowerXVI = asset TheTowerXVI Cards.theTowerXVI

instance HasModifiersFor TheTowerXVI where
  getModifiersFor (TheTowerXVI a) = case a.placement of
    StillInHand iid -> modified_ a iid [CannotCommitCards AnyCard]
    _ -> pure mempty

instance RunMessage TheTowerXVI where
  runMessage msg (TheTowerXVI attrs) = TheTowerXVI <$> runMessage msg attrs
