module Arkham.Asset.Assets.WendysAmulet (wendysAmulet) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype WendysAmulet = WendysAmulet AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wendysAmulet :: AssetCard WendysAmulet
wendysAmulet = asset WendysAmulet Cards.wendysAmulet

instance HasModifiersFor WendysAmulet where
  getModifiersFor (WendysAmulet a) = for_ a.controller \iid -> do
    controllerGets a [CanPlayTopmostOfDiscard (Just EventType, [])]
    modifySelect a (EventOwnedBy $ InvestigatorWithId iid) [PlaceOnBottomOfDeckInsteadOfDiscard]

instance RunMessage WendysAmulet where
  runMessage msg (WendysAmulet attrs) = WendysAmulet <$> runMessage msg attrs
