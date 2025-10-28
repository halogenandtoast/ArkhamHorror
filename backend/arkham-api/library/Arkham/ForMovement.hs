{-# LANGUAGE TemplateHaskell #-}

module Arkham.ForMovement where

import Arkham.Prelude
import Data.Aeson.TH

data ForMovement = ForMovement | NotForMovement
  deriving stock (Eq, Show, Ord, Data)

$(deriveJSON defaultOptions ''ForMovement)
