{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Asset where

import Arkham.Asset.Types
import {-# SOURCE #-} Arkham.Card
import Arkham.Id
import Arkham.Prelude

instance FromJSON Asset

createAsset :: IsCard a => a -> AssetId -> Asset
