{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Clue where

import Arkham.Id
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH

-- | Messages dealing with clue placement, movement, and removal.
data ClueMessage
  = PlaceCluesUpToClueValue_ LocationId Source Int
  | MoveAllCluesTo_ Source Target
  | RemoveAllClues_ Source Target
  | FlipClues_ Target Int
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''ClueMessage)
