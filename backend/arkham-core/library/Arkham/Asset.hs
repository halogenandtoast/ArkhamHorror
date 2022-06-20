{-# LANGUAGE TemplateHaskell #-}
module Arkham.Asset where

import Arkham.Prelude

import Arkham.Asset.Assets
import Arkham.Asset.Runner hiding (assetEnemy, assetLocation)
import Arkham.Card
import Arkham.Classes.Entity.TH
import Arkham.Id
import Arkham.Matcher
import Data.Aeson.TH

$(buildEntity "Asset")
$(deriveJSON defaultOptions ''Asset)

createAsset :: IsCard a => a -> Asset
createAsset a = lookupAsset (toCardCode a) (AssetId $ toCardId a)

instance HasAbilities Asset where
  getAbilities = $(entityF "Asset" 'getAbilities)

instance HasModifiersFor Asset where
  getModifiersFor = $(entityF2 "Asset" 'getModifiersFor)

instance RunMessage Asset where
  runMessage msg x = do
    inPlay <- member (toId x) <$> select AnyAsset
    modifiers' <- if inPlay
      then getModifiers (toSource x) (toTarget x)
      else pure []
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    $(entityRunMessage "Asset") msg' x

instance Entity Asset where
  type EntityId Asset = AssetId
  type EntityAttrs Asset = AssetAttrs
  toId = toId . toAttrs
  toAttrs = $(entityF "Asset" 'toAttrs)

instance TargetEntity Asset where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Asset where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

lookupAsset :: CardCode -> (AssetId -> Asset)
lookupAsset cardCode =
  fromJustNote ("Unknown asset: " <> show cardCode) $ lookup cardCode allAssets

allAssets :: HashMap CardCode (AssetId -> Asset)
allAssets = mapFromList $ map
  (cbCardCode &&& cbCardBuilder)
  $(buildEntityLookupList "Asset")
