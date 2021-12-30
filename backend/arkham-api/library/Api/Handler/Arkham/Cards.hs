module Api.Handler.Arkham.Cards
  ( getApiV1ArkhamCardsR
  ) where

import Import

import Arkham.PlayerCard
import Arkham.Card.CardCode
import Arkham.Card.CardDef

getApiV1ArkhamCardsR :: Handler [CardDef]
getApiV1ArkhamCardsR =
  pure $ filter ((/= "01000") . toCardCode) $ toList allPlayerCards
