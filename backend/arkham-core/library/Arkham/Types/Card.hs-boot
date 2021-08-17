module Arkham.Types.Card where

import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.PlayerCard

data Card = PlayerCard PlayerCard | EncounterCard EncounterCard
