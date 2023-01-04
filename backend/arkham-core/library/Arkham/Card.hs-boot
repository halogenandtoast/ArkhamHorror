module Arkham.Card where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Card.CardDef
import {-# SOURCE #-} Arkham.Card.EncounterCard
import {-# SOURCE #-} Arkham.Card.PlayerCard

data Card

instance Show Card
instance Eq Card
instance Hashable Card
instance ToJSON Card
instance FromJSON Card

class MonadRandom m => CardGen m where
  genEncounterCard :: HasCardDef a => a -> m EncounterCard
  genPlayerCard :: HasCardDef a => a -> m PlayerCard
