{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.LeoDeLuca where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype LeoDeLuca = LeoDeLuca Attrs
  deriving newtype (Show, ToJSON, FromJSON)

leoDeLuca :: AssetId -> LeoDeLuca
leoDeLuca uuid = LeoDeLuca $ (baseAttrs uuid "01048")
  { assetSlots = [AllySlot]
  , assetHealth = Just 2
  , assetSanity = Just 2
  }

instance IsInvestigator investigator => HasModifiersFor env investigator LeoDeLuca where
  getModifiersFor _ i (LeoDeLuca a) =
    pure [ AdditionalActions 1 | ownedBy a i ]

instance HasActions env investigator LeoDeLuca where
  getActions i window (LeoDeLuca x) = getActions i window x

instance (AssetRunner env) => RunMessage env LeoDeLuca where
  runMessage msg (LeoDeLuca attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage $ GainActions iid (AssetSource aid) 1
      LeoDeLuca <$> runMessage msg attrs
    _ -> LeoDeLuca <$> runMessage msg attrs
