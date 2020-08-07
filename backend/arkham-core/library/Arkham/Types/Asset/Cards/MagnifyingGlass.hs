{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.MagnifyingGlass where

import Arkham.Json
import qualified Arkham.Types.Action as Action
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

newtype MagnifyingGlassI = MagnifyingGlassI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

magnifyingGlass :: AssetId -> MagnifyingGlassI
magnifyingGlass uuid =
  MagnifyingGlassI $ (baseAttrs uuid "01030") { assetSlots = [HandSlot] }

instance (AssetRunner env) => RunMessage env MagnifyingGlassI where
  runMessage msg (MagnifyingGlassI attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage
        (AddModifier
          (InvestigatorTarget iid)
          (ActionSkillModifier
            Action.Investigate
            SkillIntellect
            1
            (AssetSource aid)
          )
        )
      MagnifyingGlassI <$> runMessage msg attrs
    _ -> MagnifyingGlassI <$> runMessage msg attrs
