{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Asset where

import Arkham.Prelude

import Arkham.Asset.Assets
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Id

createAsset :: IsCard a => a -> Asset
createAsset a =
  lookupAsset (toCardCode a) (AssetId $ toCardId a, toCardOwner a)

lookupAsset :: CardCode -> ((AssetId, Maybe InvestigatorId) -> Asset)
lookupAsset cardCode = case lookup cardCode allAssets of
  Nothing -> error $ "Unknown asset: " <> show cardCode
  Just (SomeAssetCard a) -> Asset <$> cbCardBuilder a

instance FromJSON Asset where
  parseJSON v = flip (withObject "Asset") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withAssetCardCode cCode $ \(_ :: AssetCard a) -> Asset <$> parseJSON @a v

withAssetCardCode
  :: CardCode -> (forall a . IsAsset a => AssetCard a -> r) -> r
withAssetCardCode cCode f = case lookup cCode allAssets of
  Nothing -> error "invalid assets"
  Just (SomeAssetCard a) -> f a

allAssets :: HashMap CardCode SomeAssetCard
allAssets = mapFromList $ map
  (toFst someAssetCardCode)
  [ -- Night of the Zealot
  --- signature [notz]
    SomeAssetCard rolands38Special
  --- guardian [notz]
  , SomeAssetCard fortyFiveAutomatic
  , SomeAssetCard physicalTraining
  , SomeAssetCard machete
  , SomeAssetCard guardDog
  --- neutral [notz]
  , SomeAssetCard knife
  , SomeAssetCard flashlight
  --- story [notz]
  , SomeAssetCard litaChantler
  ]
