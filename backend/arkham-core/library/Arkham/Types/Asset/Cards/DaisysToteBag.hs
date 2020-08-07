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

newtype DaisysToteBagI = DaisysToteBagI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

daisysToteBag :: AssetId -> DaisysToteBagI
daisysToteBag uuid = DaisysToteBagI $ baseAttrs uuid "01008"

instance (AssetRunner env) => RunMessage env DaisysToteBagI where
  runMessage msg (DaisysToteBagI attrs@Attrs {..}) = case msg of
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
      DaisysToteBagI <$> runMessage msg attrs
    _ -> DaisysToteBagI <$> runMessage msg attrs
