{-# LANGUAGE TemplateHaskell #-}

module Arkham.Cost.Status where

import {-# SOURCE #-} Arkham.Cost
import Arkham.Prelude
import Data.Aeson.TH

data ActionStatus = NoAction | NeedsAction
  deriving stock (Eq, Show, Ord, Data)

data CostStatus = UnpaidCost ActionStatus | PaidCost | AuxiliaryCost Cost CostStatus
  deriving stock (Eq, Show, Ord, Data)

$(deriveJSON defaultOptions ''ActionStatus)
$(deriveJSON defaultOptions ''CostStatus)
