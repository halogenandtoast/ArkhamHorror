{-# LANGUAGE TemplateHaskell #-}

module Arkham.Collection where

import Arkham.Deck
import Arkham.Prelude
import Arkham.Scenario.Deck
import Data.Aeson.TH

collectionToScenarioDeckKey :: Collection -> Maybe ScenarioDeckKey
collectionToScenarioDeckKey (DeckCollection (ScenarioDeckByKey k)) = Just k
collectionToScenarioDeckKey _ = Nothing

class IsCollection a where
  toCollection :: a -> Collection

newtype Collection = DeckCollection DeckSignifier
  deriving stock (Show, Eq, Ord, Data)

instance IsCollection DeckSignifier where
  toCollection = DeckCollection
  {-# INLINE toCollection #-}

instance IsCollection ScenarioDeckKey where
  toCollection = toCollection . ScenarioDeckByKey
  {-# INLINE toCollection #-}

instance IsCollection Collection where
  toCollection = id
  {-# INLINE toCollection #-}

$(deriveJSON defaultOptions ''Collection)
