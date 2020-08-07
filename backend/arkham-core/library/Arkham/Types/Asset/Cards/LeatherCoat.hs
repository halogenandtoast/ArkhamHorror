{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.LeatherCoat where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Slot
import ClassyPrelude

newtype LeatherCoatI = LeatherCoatI Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

leatherCoat :: AssetId -> LeatherCoatI
leatherCoat uuid = LeatherCoatI
  $ (baseAttrs uuid "01072") { assetSlots = [BodySlot], assetHealth = Just 2 }

instance (AssetRunner env) => RunMessage env LeatherCoatI where
  runMessage msg (LeatherCoatI attrs) = LeatherCoatI <$> runMessage msg attrs
