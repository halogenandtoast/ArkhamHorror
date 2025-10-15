{-# LANGUAGE TemplateHaskell #-}

module Arkham.Campaigns.TheScarletKeys.Key.Stability where

import Arkham.Prelude
import Data.Aeson.TH

data Stability = Stable | Unstable
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''Stability)
