{-# LANGUAGE TemplateHaskell #-}

module Arkham.Cost.Status where

import Arkham.Prelude

import Data.Aeson.TH

data CostStatus = UnpaidCost | PaidCost
  deriving stock (Eq, Show, Ord, Data)

$(deriveJSON defaultOptions ''CostStatus)
