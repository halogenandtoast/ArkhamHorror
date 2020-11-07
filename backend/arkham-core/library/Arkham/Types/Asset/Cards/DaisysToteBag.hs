{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.DaisysToteBag
  ( DaisysToteBag(..)
  , daisysToteBag
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Trait

newtype DaisysToteBag = DaisysToteBag Attrs
  deriving newtype (Show, ToJSON, FromJSON)

daisysToteBag :: AssetId -> DaisysToteBag
daisysToteBag uuid = DaisysToteBag $ baseAttrs uuid "01008" $ pure ()

instance HasModifiersFor env DaisysToteBag where
  getModifiersFor _ _ _ = pure []

instance HasActions env DaisysToteBag where
  getActions i window (DaisysToteBag x) = getActions i window x

slot :: Attrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome Nothing

instance (HasQueue env, HasModifiers env InvestigatorId) => RunMessage env DaisysToteBag where
  runMessage msg (DaisysToteBag attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      unshiftMessages $ replicate 2 (AddSlot iid HandSlot (slot attrs))
      DaisysToteBag <$> runMessage msg attrs
    _ -> DaisysToteBag <$> runMessage msg attrs
