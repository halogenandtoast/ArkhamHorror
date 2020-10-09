{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.MagnifyingGlass where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype MagnifyingGlass = MagnifyingGlass Attrs
  deriving newtype (Show, ToJSON, FromJSON)

magnifyingGlass :: AssetId -> MagnifyingGlass
magnifyingGlass uuid =
  MagnifyingGlass $ (baseAttrs uuid "01030") { assetSlots = [HandSlot] }

instance IsInvestigator investigator => HasModifiersFor env investigator MagnifyingGlass where
  getModifiersFor _ i (MagnifyingGlass a) = pure
    [ ActionSkillModifier Action.Investigate SkillIntellect 1 | ownedBy a i ]

instance HasActions env investigator MagnifyingGlass where
  getActions i window (MagnifyingGlass x) = getActions i window x

instance (AssetRunner env) => RunMessage env MagnifyingGlass where
  runMessage msg (MagnifyingGlass attrs) =
    MagnifyingGlass <$> runMessage msg attrs
