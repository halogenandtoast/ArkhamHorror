module Arkham.Card where

import Arkham.Prelude

import Arkham.EncounterCard
import Arkham.PlayerCard
import Arkham.Types.Card
import Arkham.Types.Card.Id

buildCard :: MonadRandom m => CardCode -> m Card
buildCard cardCode = lookupCard cardCode <$> getRandom

lookupCard :: CardCode -> CardId -> Card
lookupCard cardCode cardId =
  case (lookup cardCode allEncounterCards, lookup cardCode allPlayerCards) of
    (Nothing, Nothing) -> error $ "Missing card " <> show cardCode
    (Just _, Just _) ->
      error $ "card is both encounter and player " <> show cardCode
    (Just _, Nothing) ->
      EncounterCard $ MkEncounterCard { ecId = cardId, ecCardCode = cardCode }
    (Nothing, Just _) -> PlayerCard $ MkPlayerCard
      { pcId = cardId
      , pcBearer = Nothing
      , pcCardCode = cardCode
      }
