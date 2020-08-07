{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.DaisysToteBag where

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
import Arkham.Types.Trait
import ClassyPrelude

newtype DaisysToteBag = DaisysToteBag Attrs
  deriving newtype (Show, ToJSON, FromJSON)

daisysToteBag :: AssetId -> DaisysToteBag
daisysToteBag uuid = DaisysToteBag $ baseAttrs uuid "01008"

instance (AssetRunner env) => RunMessage env DaisysToteBag where
  runMessage msg (DaisysToteBag attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessages
        [ AddModifier
          (InvestigatorTarget iid)
          (AddSlot HandSlot (TraitRestrictedSlot Tome Nothing) (AssetSource aid)
          )
        , AddModifier
          (InvestigatorTarget iid)
          (AddSlot HandSlot (TraitRestrictedSlot Tome Nothing) (AssetSource aid)
          )
        ]
      DaisysToteBag <$> runMessage msg attrs
    _ -> DaisysToteBag <$> runMessage msg attrs
