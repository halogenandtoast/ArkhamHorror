module Arkham.Card.PlayerCard where

import Arkham.Prelude

data PlayerCard

instance NoThunks PlayerCard
instance Data PlayerCard
instance Ord PlayerCard
instance FromJSON PlayerCard
instance ToJSON PlayerCard
instance Show PlayerCard
instance Eq PlayerCard
