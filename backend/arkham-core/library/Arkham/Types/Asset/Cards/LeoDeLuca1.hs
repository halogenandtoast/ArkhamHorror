{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.LeoDeLuca1 where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype LeoDeLuca1 = LeoDeLuca1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

leoDeLuca1 :: AssetId -> LeoDeLuca1
leoDeLuca1 uuid = LeoDeLuca1 $ baseAttrs uuid "01054" $ do
  slots .= [AllySlot]
  health ?= 2
  sanity ?= 2

instance HasModifiersFor env LeoDeLuca1 where
  getModifiersFor _ (InvestigatorTarget iid) (LeoDeLuca1 a) =
    pure [ AdditionalActions 1 | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env LeoDeLuca1 where
  getActions i window (LeoDeLuca1 x) = getActions i window x

instance (AssetRunner env) => RunMessage env LeoDeLuca1 where
  runMessage msg (LeoDeLuca1 attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage $ GainActions iid (AssetSource aid) 1
      LeoDeLuca1 <$> runMessage msg attrs
    _ -> LeoDeLuca1 <$> runMessage msg attrs
