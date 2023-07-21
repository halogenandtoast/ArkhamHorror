{-# LANGUAGE TemplateHaskell #-}

module Arkham.LocationSymbol where

import Arkham.Prelude

import Data.Aeson.TH

data LocationSymbol
  = Circle
  | Square
  | Triangle
  | Plus
  | Diamond
  | Squiggle
  | Moon
  | Hourglass
  | T
  | Equals
  | Heart
  | Star
  | Droplet
  | Trefoil
  | NoSymbol
  deriving stock (Show, Ord, Eq)

$(deriveJSON defaultOptions ''LocationSymbol)
