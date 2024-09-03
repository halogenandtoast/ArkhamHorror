module Arkham.Asset.Cards.WendysAmuletAdvanced where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Event.Types (Field (..))
import Arkham.Projection

newtype WendysAmuletAdvanced = WendysAmuletAdvanced AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wendysAmuletAdvanced :: AssetCard WendysAmuletAdvanced
wendysAmuletAdvanced = asset WendysAmuletAdvanced Cards.wendysAmuletAdvanced

instance HasModifiersFor WendysAmuletAdvanced where
  getModifiersFor (InvestigatorTarget iid) (WendysAmuletAdvanced a) | controlledBy a iid = do
    pure $ toModifiers a [CanPlayFromDiscard (Just EventType, [])]
  getModifiersFor (EventTarget eid) (WendysAmuletAdvanced a) = do
    owner <- field EventOwner eid
    pure $ toModifiers a [PlaceOnBottomOfDeckInsteadOfDiscard | controlledBy a owner]
  getModifiersFor _ _ = pure []

instance RunMessage WendysAmuletAdvanced where
  runMessage msg (WendysAmuletAdvanced attrs) = WendysAmuletAdvanced <$> runMessage msg attrs
