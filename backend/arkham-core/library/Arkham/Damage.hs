{-# LANGUAGE TemplateHaskell #-}

module Arkham.Damage where

import Arkham.Prelude

import Data.Aeson.TH

data DamageType = HorrorType | DamageType
  deriving stock (Show, Eq, Ord)

$(deriveJSON defaultOptions ''DamageType)
