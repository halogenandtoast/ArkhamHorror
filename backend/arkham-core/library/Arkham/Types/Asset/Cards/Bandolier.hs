{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Bandolier where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Trait
import ClassyPrelude

newtype Bandolier = Bandolier Attrs
  deriving newtype (Show, ToJSON, FromJSON)

bandolier :: AssetId -> Bandolier
bandolier uuid = Bandolier
  $ (baseAttrs uuid "02147") { assetHealth = Just 1, assetSlots = [BodySlot] }

instance HasModifiersFor env investigator Bandolier where
  getModifiersFor _ _ = pure []

instance HasActions env investigator Bandolier where
  getActions i window (Bandolier x) = getActions i window x

instance (AssetRunner env) => RunMessage env Bandolier where
  runMessage msg (Bandolier attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessages
        [ AddSlot
            iid
            HandSlot
            (TraitRestrictedSlot (AssetSource aid) Weapon Nothing)
        ]
      Bandolier <$> runMessage msg attrs
    _ -> Bandolier <$> runMessage msg attrs
