{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Bandolier
  ( Bandolier(..)
  , bandolier
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype Bandolier = Bandolier Attrs
  deriving newtype (Show, ToJSON, FromJSON)

bandolier :: AssetId -> Bandolier
bandolier uuid = Bandolier
  $ (baseAttrs uuid "02147") { assetHealth = Just 1, assetSlots = [BodySlot] }

instance HasModifiersFor env investigator Bandolier where
  getModifiersFor _ _ _ = pure []

instance HasActions env investigator Bandolier where
  getActions i window (Bandolier x) = getActions i window x

slot :: Attrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Weapon Nothing

instance AssetRunner env => RunMessage env Bandolier where
  runMessage msg (Bandolier attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      unshiftMessage $ AddSlot iid HandSlot (slot attrs)
      Bandolier <$> runMessage msg attrs
    _ -> Bandolier <$> runMessage msg attrs
