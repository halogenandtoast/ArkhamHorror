{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.HolyRosary where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype HolyRosary = HolyRosary Attrs
  deriving newtype (Show, ToJSON, FromJSON)

holyRosary :: AssetId -> HolyRosary
holyRosary uuid = HolyRosary $ baseAttrs uuid "01059" $ do
  slots .= [AccessorySlot]
  sanity ?= 2

instance HasModifiersFor env  HolyRosary where
  getModifiersFor _ (InvestigatorTarget iid) (HolyRosary a) =
    pure [ SkillModifier SkillWillpower 1 | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env HolyRosary where
  getActions i window (HolyRosary x) = getActions i window x

instance AssetRunner env => RunMessage env HolyRosary where
  runMessage msg (HolyRosary attrs) = HolyRosary <$> runMessage msg attrs
