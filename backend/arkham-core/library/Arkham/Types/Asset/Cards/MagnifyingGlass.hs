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

newtype MagnifyingGlass = MagnifyingGlass Attrs
  deriving newtype (Show, ToJSON, FromJSON)

magnifyingGlass :: AssetId -> MagnifyingGlass
magnifyingGlass uuid =
  MagnifyingGlass $ (baseAttrs uuid "01030") { assetSlots = [HandSlot] }

instance (IsInvestigator investigator) => HasActions investigator MagnifyingGlass where
  getActions i (MagnifyingGlass x) = getActions i x

instance (AssetRunner env) => RunMessage env MagnifyingGlass where
  runMessage msg (MagnifyingGlass attrs@Attrs {..}) = case msg of
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
      MagnifyingGlass <$> runMessage msg attrs
    _ -> MagnifyingGlass <$> runMessage msg attrs
