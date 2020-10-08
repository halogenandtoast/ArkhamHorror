{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.BulletproofVest3 where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Slot
import ClassyPrelude

newtype BulletproofVest3 = BulletproofVest3 Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

bulletproofVest3 :: AssetId -> BulletproofVest3
bulletproofVest3 uuid = BulletproofVest3
  $ (baseAttrs uuid "01094") { assetSlots = [BodySlot], assetHealth = Just 4 }

instance HasModifiersFor env investigator BulletproofVest3 where
  getModifiersFor _ _ _ = pure []

instance HasActions env investigator BulletproofVest3 where
  getActions i window (BulletproofVest3 x) = getActions i window x

instance (AssetRunner env) => RunMessage env BulletproofVest3 where
  runMessage msg (BulletproofVest3 attrs) =
    BulletproofVest3 <$> runMessage msg attrs
