module Arkham.Asset.Cards.WendysAmulet where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Event.Types (Field (..))
import Arkham.Projection

newtype WendysAmulet = WendysAmulet AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wendysAmulet :: AssetCard WendysAmulet
wendysAmulet = asset WendysAmulet Cards.wendysAmulet

instance HasModifiersFor WendysAmulet where
  getModifiersFor (InvestigatorTarget iid) (WendysAmulet a) | controlledBy a iid = do
    pure $ toModifiers a [CanPlayTopmostOfDiscard (Just EventType, [])]
  getModifiersFor (EventTarget eid) (WendysAmulet a) = do
    owner <- field EventOwner eid
    pure $ toModifiers a [PlaceOnBottomOfDeckInsteadOfDiscard | controlledBy a owner]
  getModifiersFor _ _ = pure []

instance RunMessage WendysAmulet where
  runMessage msg (WendysAmulet attrs) = WendysAmulet <$> runMessage msg attrs
