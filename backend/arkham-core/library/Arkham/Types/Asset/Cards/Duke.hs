{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Duke where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import ClassyPrelude

newtype Duke = Duke Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

duke :: AssetId -> Duke
duke uuid =
  Duke $ (baseAttrs uuid "02014") { assetHealth = Just 2, assetSanity = Just 3 }

instance HasActions env investigator Duke where
  getActions i window (Duke x) = getActions i window x

instance (AssetRunner env) => RunMessage env Duke where
  runMessage msg (Duke attrs) = Duke <$> runMessage msg attrs
