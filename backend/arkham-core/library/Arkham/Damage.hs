{-# LANGUAGE TemplateHaskell #-}

module Arkham.Damage where

import Arkham.Prelude

import Data.Aeson.TH
import GHC.OverloadedLabels

data DamageType = HorrorType | DamageType
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (NoThunks)

instance IsLabel "horror" DamageType where
  fromLabel = HorrorType

instance IsLabel "damage" DamageType where
  fromLabel = DamageType

$(deriveJSON defaultOptions ''DamageType)
