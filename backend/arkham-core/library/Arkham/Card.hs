module Arkham.Card where

import Arkham.Prelude

import Arkham.EncounterCard
import Arkham.PlayerCard
import {-# SOURCE #-} Arkham.Types.Card
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard

lookupCard :: CardCode -> CardId -> Card
lookupCard cardCode cardId =
  case (lookup cardCode allEncounterCards, lookup cardCode allPlayerCards) of
    (Nothing, Nothing) -> error $ "Missing card " <> show cardCode
    (Just def, _) -> EncounterCard $ lookupEncounterCard def cardId
    -- we prefer encounter cards over player cards to handle cases like straitjacket
    (Nothing, Just def) -> PlayerCard $ lookupPlayerCard def cardId
