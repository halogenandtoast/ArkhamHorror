module Arkham.Types.Card where

import {-# SOURCE #-} Arkham.Types.Card.EncounterCard
import {-# SOURCE #-} Arkham.Types.Card.PlayerCard

data Card = PlayerCard PlayerCard | EncounterCard EncounterCard
