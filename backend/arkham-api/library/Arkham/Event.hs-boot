{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Event where

import Arkham.Prelude

import Arkham.Event.Types

instance FromJSON Event
