{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.LeatherCoat where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Slot
import ClassyPrelude

newtype LeatherCoat = LeatherCoat Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

leatherCoat :: AssetId -> LeatherCoat
leatherCoat uuid = LeatherCoat
  $ (baseAttrs uuid "01072") { assetSlots = [BodySlot], assetHealth = Just 2 }

instance (IsInvestigator investigator) => HasActions env investigator LeatherCoat where
  getActions i (LeatherCoat x) = getActions i x

instance (AssetRunner env) => RunMessage env LeatherCoat where
  runMessage msg (LeatherCoat attrs) = LeatherCoat <$> runMessage msg attrs
