{-# LANGUAGE TemplateHaskell #-}

module Arkham.Cost.Status where

import Arkham.Prelude

import Data.Aeson.TH

data CostStatus = UnpaidCost | PaidCost
  deriving stock (Eq, Show, Ord, Data, Generic)
  deriving anyclass (NoThunks, NFData)

$(deriveJSON defaultOptions ''CostStatus)
