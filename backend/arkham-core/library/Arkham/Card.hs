module Arkham.Card where

import Arkham.Prelude

import Arkham.EncounterCard
import Arkham.PlayerCard
import {-# SOURCE #-} Arkham.Types.Card
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard

buildCard :: MonadRandom m => CardCode -> m Card
buildCard cardCode = lookupCard cardCode <$> getRandom

lookupCard :: CardCode -> CardId -> Card
lookupCard cardCode cardId =
  case (lookup cardCode allEncounterCards, lookup cardCode allPlayerCards) of
    (Nothing, Nothing) -> error $ "Missing card " <> show cardCode
    (Just _, Just _) ->
      error $ "card is both encounter and player " <> show cardCode
    (Just def, Nothing) -> EncounterCard $ lookupEncounterCard def cardId
    (Nothing, Just def) -> PlayerCard $ lookupPlayerCard def cardId
