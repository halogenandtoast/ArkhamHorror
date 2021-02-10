module Arkham.Card where

import Arkham.Prelude

import Arkham.EncounterCard
import Arkham.PlayerCard
import Arkham.Types.Card
import Arkham.Types.Card.Id

buildCard :: MonadRandom m => CardCode -> m Card
buildCard cardCode = lookupCard cardCode <$> getRandom

lookupCard :: CardCode -> (CardId -> Card)
lookupCard cardCode =
  let
    encounterCard = do
      f <- lookup cardCode allEncounterCards
      pure $ EncounterCard . f
    playerCard = do
      f <- lookup cardCode allPlayerCards
      pure $ PlayerCard . f
  in
    fromJustNote ("Missing card " <> show cardCode)
    $ encounterCard
    <|> playerCard
