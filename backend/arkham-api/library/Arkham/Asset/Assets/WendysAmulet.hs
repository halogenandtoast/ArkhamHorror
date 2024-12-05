module Arkham.Asset.Assets.WendysAmulet where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher

newtype WendysAmulet = WendysAmulet AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wendysAmulet :: AssetCard WendysAmulet
wendysAmulet = asset WendysAmulet Cards.wendysAmulet

instance HasModifiersFor WendysAmulet where
  getModifiersFor (WendysAmulet a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      controller <- controllerGets a [CanPlayTopmostOfDiscard (Just EventType, [])]
      events <-
        modifySelect a (EventOwnedBy $ InvestigatorWithId iid) [PlaceOnBottomOfDeckInsteadOfDiscard]
      pure $ controller <> events

instance RunMessage WendysAmulet where
  runMessage msg (WendysAmulet attrs) = WendysAmulet <$> runMessage msg attrs
