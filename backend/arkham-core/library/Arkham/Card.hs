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
      pure $ \cardId -> EncounterCard $ MkEncounterCard
        { ecId = cardId
        , ecDef = f
        }
    playerCard = do
      f <- lookup cardCode allPlayerCards
      pure $ \cardId -> PlayerCard $ MkPlayerCard
        { pcId = cardId
        , pcDef = f
        , pcBearer = Nothing
        }
  in
    fromJustNote ("Missing card " <> show cardCode)
    $ encounterCard
    <|> playerCard
