{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.LeoDeLuca1 where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Slot
import Arkham.Types.Source
import ClassyPrelude


newtype LeoDeLuca1 = LeoDeLuca1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

leoDeLuca1 :: AssetId -> LeoDeLuca1
leoDeLuca1 uuid = LeoDeLuca1 $ (baseAttrs uuid "01054")
  { assetSlots = [AllySlot]
  , assetHealth = Just 2
  , assetSanity = Just 2
  }

instance IsInvestigator investigator => HasModifiersFor env investigator LeoDeLuca1 where
  getModifiersFor i (LeoDeLuca1 Attrs {..}) =
    pure [ AdditionalActions 1 | Just (getId () i) == assetInvestigator ]

instance HasActions env investigator LeoDeLuca1 where
  getActions i window (LeoDeLuca1 x) = getActions i window x

instance (AssetRunner env) => RunMessage env LeoDeLuca1 where
  runMessage msg (LeoDeLuca1 attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage $ GainActions iid (AssetSource aid) 1
      LeoDeLuca1 <$> runMessage msg attrs
    _ -> LeoDeLuca1 <$> runMessage msg attrs
