{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Token where

import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Token (Token)
import Data.Aeson.TH

-- | Messages dealing with generic token placement and movement on game entities.
data TokenMessage
  = PlaceTokens_ Source Target Token Int
  | RemoveTokens_ Source Target Token Int
  | ClearTokens_ Target
  | MoveTokens_ Source Source Target Token Int
  | MoveTokensNoDefeated_ Source Source Target Token Int
  | RemoveAllTokens_ Source Target
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''TokenMessage)
