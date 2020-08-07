{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.HolyRosary where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude

newtype HolyRosaryI = HolyRosaryI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

holyRosary :: AssetId -> HolyRosaryI
holyRosary uuid = HolyRosaryI $ (baseAttrs uuid "01059")
  { assetSlots = [AccessorySlot]
  , assetSanity = Just 2
  }

instance (AssetRunner env) => RunMessage env HolyRosaryI where
  runMessage msg (HolyRosaryI attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage
        (AddModifier
          (InvestigatorTarget iid)
          (SkillModifier SkillWillpower 1 (AssetSource aid))
        )
      HolyRosaryI <$> runMessage msg attrs
    _ -> HolyRosaryI <$> runMessage msg attrs
