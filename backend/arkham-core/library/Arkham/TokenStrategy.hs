{-# LANGUAGE TemplateHaskell #-}

module Arkham.TokenStrategy where

import Arkham.Json
import Arkham.Prelude
import Data.Aeson.TH

data TokenStrategy = DefaultTokenStrategy | DrawXResolveOne Int
  deriving stock (Show, Eq)

$(deriveJSON defaultOptions ''TokenStrategy)
