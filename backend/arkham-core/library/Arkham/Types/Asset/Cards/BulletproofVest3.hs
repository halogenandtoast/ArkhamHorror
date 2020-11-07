{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.BulletproofVest3 where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype BulletproofVest3 = BulletproofVest3 Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

bulletproofVest3 :: AssetId -> BulletproofVest3
bulletproofVest3 uuid = BulletproofVest3 $ baseAttrs uuid "01094" $ do
  slots .= [BodySlot]
  health ?= 4

instance HasModifiersFor env BulletproofVest3 where
  getModifiersFor _ _ _ = pure []

instance HasActions env BulletproofVest3 where
  getActions i window (BulletproofVest3 x) = getActions i window x

instance (AssetRunner env) => RunMessage env BulletproofVest3 where
  runMessage msg (BulletproofVest3 attrs) =
    BulletproofVest3 <$> runMessage msg attrs
