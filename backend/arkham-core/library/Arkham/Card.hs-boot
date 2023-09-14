module Arkham.Card where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Card.CardDef
import {-# SOURCE #-} Arkham.Card.EncounterCard
import Arkham.Card.Id
import {-# SOURCE #-} Arkham.Card.PlayerCard

data Card

instance Data Card
instance Show Card
instance Eq Card
instance Ord Card
instance ToJSON Card
instance FromJSON Card

class MonadRandom m => CardGen m where
  genEncounterCard :: HasCardDef a => a -> m EncounterCard
  genPlayerCard :: HasCardDef a => a -> m PlayerCard
  replaceCard :: CardId -> Card -> m ()
