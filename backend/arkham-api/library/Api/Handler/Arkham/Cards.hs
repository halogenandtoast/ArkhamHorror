module Api.Handler.Arkham.Cards
  ( getApiV1ArkhamCardsR
  ) where

import Import

import Arkham.PlayerCard
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef

getApiV1ArkhamCardsR :: Handler [CardDef]
getApiV1ArkhamCardsR =
  pure $ filter ((/= "01000") . toCardCode) $ toList allPlayerCards
