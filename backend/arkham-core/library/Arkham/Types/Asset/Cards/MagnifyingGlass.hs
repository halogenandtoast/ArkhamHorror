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
  MagnifyingGlass $ baseAttrs uuid "01030" $ slots .= [HandSlot]

instance HasModifiersFor env MagnifyingGlass where
  getModifiersFor _ (InvestigatorTarget iid) (MagnifyingGlass a) =
    pure
      [ ActionSkillModifier Action.Investigate SkillIntellect 1
      | ownedBy a iid
      ]
  getModifiersFor _ _ _ = pure []

instance HasActions env MagnifyingGlass where
  getActions i window (MagnifyingGlass x) = getActions i window x

instance (AssetRunner env) => RunMessage env MagnifyingGlass where
  runMessage msg (MagnifyingGlass attrs) =
    MagnifyingGlass <$> runMessage msg attrs
