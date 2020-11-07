{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.LeatherCoat where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype LeatherCoat = LeatherCoat Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

leatherCoat :: AssetId -> LeatherCoat
leatherCoat uuid = LeatherCoat $ baseAttrs uuid "01072" $ do
  slots .= [BodySlot]
  health ?= 2

instance HasModifiersFor env LeatherCoat where
  getModifiersFor _ _ _ = pure []

instance HasActions env LeatherCoat where
  getActions i window (LeatherCoat x) = getActions i window x

instance (AssetRunner env) => RunMessage env LeatherCoat where
  runMessage msg (LeatherCoat attrs) = LeatherCoat <$> runMessage msg attrs
