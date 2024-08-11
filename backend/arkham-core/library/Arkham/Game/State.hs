{-# LANGUAGE TemplateHaskell #-}

module Arkham.Game.State where

import Arkham.Id
import Arkham.Prelude
import Data.Aeson.TH

data GameState = IsPending [PlayerId] | IsChooseDecks [PlayerId] | IsActive | IsOver
  deriving stock (Eq, Show, Data)

isChooseDecks :: GameState -> Bool
isChooseDecks = \case
  IsChooseDecks _ -> True
  _ -> False

$(deriveJSON defaultOptions ''GameState)
