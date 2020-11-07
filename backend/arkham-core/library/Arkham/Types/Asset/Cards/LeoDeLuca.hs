{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.LeoDeLuca where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype LeoDeLuca = LeoDeLuca Attrs
  deriving newtype (Show, ToJSON, FromJSON)

leoDeLuca :: AssetId -> LeoDeLuca
leoDeLuca uuid = LeoDeLuca $ baseAttrs uuid "01048" $ do
  slots .= [AllySlot]
  health ?= 2
  sanity ?= 2

instance HasModifiersFor env LeoDeLuca where
  getModifiersFor _ (InvestigatorTarget iid) (LeoDeLuca a) =
    pure [ AdditionalActions 1 | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env LeoDeLuca where
  getActions i window (LeoDeLuca x) = getActions i window x

instance (AssetRunner env) => RunMessage env LeoDeLuca where
  runMessage msg (LeoDeLuca attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage $ GainActions iid (AssetSource aid) 1
      LeoDeLuca <$> runMessage msg attrs
    _ -> LeoDeLuca <$> runMessage msg attrs
