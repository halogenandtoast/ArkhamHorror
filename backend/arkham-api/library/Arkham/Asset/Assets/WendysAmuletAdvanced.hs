module Arkham.Asset.Assets.WendysAmuletAdvanced where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher

newtype WendysAmuletAdvanced = WendysAmuletAdvanced AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wendysAmuletAdvanced :: AssetCard WendysAmuletAdvanced
wendysAmuletAdvanced = asset WendysAmuletAdvanced Cards.wendysAmuletAdvanced

instance HasModifiersFor WendysAmuletAdvanced where
  getModifiersFor (WendysAmuletAdvanced a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      controller <- controllerGets a [CanPlayFromDiscard (Just EventType, [])]
      events <-
        modifySelect a (EventOwnedBy $ InvestigatorWithId iid) [PlaceOnBottomOfDeckInsteadOfDiscard]
      pure $ controller <> events

instance RunMessage WendysAmuletAdvanced where
  runMessage msg (WendysAmuletAdvanced attrs) = WendysAmuletAdvanced <$> runMessage msg attrs
