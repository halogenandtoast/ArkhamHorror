module Arkham.Asset.Cards.TheTowerXVI (theTowerXVI, TheTowerXVI (..)) where

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
  getModifiersFor (InvestigatorTarget iid) (TheTowerXVI attrs)
    | attrs.placement == StillInHand iid =
        pure $ toModifiers attrs [CannotCommitCards AnyCard]
  getModifiersFor _ _ = pure []

instance RunMessage TheTowerXVI where
  runMessage msg (TheTowerXVI attrs) = TheTowerXVI <$> runMessage msg attrs
