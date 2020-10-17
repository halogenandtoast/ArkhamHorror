{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.BearTrap
  ( BearTrap(..)
  , bearTrap
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype BearTrap = BearTrap Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

bearTrap :: AssetId -> BearTrap
bearTrap uuid = BearTrap $ baseAttrs uuid "81020"

instance HasModifiersFor env BearTrap where
  getModifiersFor _ _ _ = pure []

instance HasActions env BearTrap where
  getActions iid window (BearTrap x) = getActions iid window x

instance (AssetRunner env) => RunMessage env BearTrap where
  runMessage msg (BearTrap attrs) = BearTrap <$> runMessage msg attrs
