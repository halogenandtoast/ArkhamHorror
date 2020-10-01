{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.MagnifyingGlass where

import Arkham.Json
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import ClassyPrelude

newtype MagnifyingGlass = MagnifyingGlass Attrs
  deriving newtype (Show, ToJSON, FromJSON)

magnifyingGlass :: AssetId -> MagnifyingGlass
magnifyingGlass uuid =
  MagnifyingGlass $ (baseAttrs uuid "01030") { assetSlots = [HandSlot] }

instance IsInvestigator investigator => HasModifiersFor env investigator MagnifyingGlass where
  getModifiersFor i (MagnifyingGlass Attrs {..}) = pure
    [ ActionSkillModifier Action.Investigate SkillIntellect 1
    | Just (getId () i) == assetInvestigator
    ]

instance HasActions env investigator MagnifyingGlass where
  getActions i window (MagnifyingGlass x) = getActions i window x

instance (AssetRunner env) => RunMessage env MagnifyingGlass where
  runMessage msg (MagnifyingGlass attrs) =
    MagnifyingGlass <$> runMessage msg attrs
