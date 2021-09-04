module Arkham.Types.Card where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Types.Card.EncounterCard
import {-# SOURCE #-} Arkham.Types.Card.PlayerCard

data Card = PlayerCard PlayerCard | EncounterCard EncounterCard

instance ToJSON Card
instance FromJSON Card
instance Hashable Card
instance Show Card
instance Eq Card
