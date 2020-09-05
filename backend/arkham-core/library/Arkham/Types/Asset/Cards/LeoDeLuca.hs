{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.LeoDeLuca where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude


newtype LeoDeLuca = LeoDeLuca Attrs
  deriving newtype (Show, ToJSON, FromJSON)

leoDeLuca :: AssetId -> LeoDeLuca
leoDeLuca uuid = LeoDeLuca $ (baseAttrs uuid "01048")
  { assetSlots = [AllySlot]
  , assetHealth = Just 2
  , assetSanity = Just 2
  }

instance HasActions env investigator LeoDeLuca where
  getActions i window (LeoDeLuca x) = getActions i window x

instance (AssetRunner env) => RunMessage env LeoDeLuca where
  runMessage msg (LeoDeLuca attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessages
        [ GainActions iid (AssetSource aid) 1
        , AddModifiers
          (InvestigatorTarget iid)
          (AssetSource aid)
          [AdditionalActions 1]
        ]
      LeoDeLuca <$> runMessage msg attrs
    _ -> LeoDeLuca <$> runMessage msg attrs
