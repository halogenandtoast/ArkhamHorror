{-# LANGUAGE TemplateHaskell #-}

module Arkham.Choose where

import Arkham.Card
import Arkham.Collection
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH
import GHC.Records

data ChooseKind = RandomlyChoose
  deriving stock (Show, Eq)

data Choose = Choose
  { chooseFrom :: Collection
  , chooseTarget :: Target
  , chooseSource :: Source
  , chooseAmount :: Int
  , chooseKind :: ChooseKind
  }
  deriving stock (Show, Eq)

instance HasField "amount" Choose Int where
  getField = chooseAmount

instance HasField "target" Choose Target where
  getField = chooseTarget

instance HasField "collection" Choose Collection where
  getField = chooseFrom

data Chosen = Chosen Choose [Card]
  deriving stock (Show, Eq)

instance HasField "cards" Chosen [Card] where
  getField (Chosen _ cards) = cards

instance HasField "target" Chosen Target where
  getField (Chosen choose _) = choose.target

finalizeChoose :: Choose -> [Card] -> Chosen
finalizeChoose = Chosen

chooseRandom :: (Targetable a, Sourceable a, IsCollection col) => a -> col -> Int -> Choose
chooseRandom a col n =
  Choose
    { chooseFrom = toCollection col
    , chooseTarget = toTarget a
    , chooseSource = toSource a
    , chooseAmount = n
    , chooseKind = RandomlyChoose
    }

$(deriveJSON defaultOptions ''ChooseKind)
$(deriveJSON defaultOptions ''Choose)
$(deriveJSON defaultOptions ''Chosen)
