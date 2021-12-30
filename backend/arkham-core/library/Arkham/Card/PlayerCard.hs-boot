module Arkham.Card.PlayerCard where

import Arkham.Prelude

data PlayerCard

instance Hashable PlayerCard
instance FromJSON PlayerCard
instance ToJSON PlayerCard
instance Show PlayerCard
instance Eq PlayerCard
