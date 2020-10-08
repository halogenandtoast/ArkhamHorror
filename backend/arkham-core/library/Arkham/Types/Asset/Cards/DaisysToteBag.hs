{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.DaisysToteBag where

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

newtype DaisysToteBag = DaisysToteBag Attrs
  deriving newtype (Show, ToJSON, FromJSON)

daisysToteBag :: AssetId -> DaisysToteBag
daisysToteBag uuid = DaisysToteBag $ baseAttrs uuid "01008"

instance HasModifiersFor env investigator DaisysToteBag where
  getModifiersFor _ _ _ = pure []

instance HasActions env investigator DaisysToteBag where
  getActions i window (DaisysToteBag x) = getActions i window x

instance (AssetRunner env) => RunMessage env DaisysToteBag where
  runMessage msg (DaisysToteBag attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessages
        [ AddSlot
          iid
          HandSlot
          (TraitRestrictedSlot (AssetSource aid) Tome Nothing)
        , AddSlot
          iid
          HandSlot
          (TraitRestrictedSlot (AssetSource aid) Tome Nothing)
        ]
      DaisysToteBag <$> runMessage msg attrs
    _ -> DaisysToteBag <$> runMessage msg attrs
