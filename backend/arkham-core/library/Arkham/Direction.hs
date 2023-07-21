{-# LANGUAGE TemplateHaskell #-}

module Arkham.Direction (
  module Arkham.Direction,
) where

import Arkham.Prelude

import Data.Aeson.TH

data Direction = Above | Below | LeftOf | RightOf
  deriving stock (Show, Eq, Ord, Enum, Bounded)

$(deriveJSON defaultOptions ''Direction)

instance ToJSONKey Direction
instance FromJSONKey Direction
