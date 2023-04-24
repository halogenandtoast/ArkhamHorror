{-# LANGUAGE TemplateHaskell #-}
module Arkham.DefeatedBy where

import Arkham.Prelude

import Data.Aeson.TH

data DefeatedBy
  = DefeatedByHorror
  | DefeatedByDamage
  | DefeatedByDamageAndHorror
  | DefeatedByOther
  deriving stock (Show, Eq, Ord)

$(deriveJSON defaultOptions ''DefeatedBy)
