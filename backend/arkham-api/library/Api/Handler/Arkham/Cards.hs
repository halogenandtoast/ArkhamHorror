module Api.Handler.Arkham.Cards
  ( getApiV1ArkhamCardsR
  ) where

import Import

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.EncounterCard
import Arkham.PlayerCard

getApiV1ArkhamCardsR :: Handler [CardDef]
getApiV1ArkhamCardsR = do
  showEncounter <- maybe False (const True)
    <$> lookupGetParam "includeEncounter"
  let
    cards = if showEncounter
      then allPlayerCards <> allEncounterCards
      else allPlayerCards
  pure $ filter ((/= "01000") . toCardCode) $ toList cards
