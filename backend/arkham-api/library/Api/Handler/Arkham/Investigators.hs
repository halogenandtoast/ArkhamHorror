module Api.Handler.Arkham.Investigators
  ( getApiV1ArkhamInvestigatorsR
  ) where

import Import

import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.Investigator.Cards

getApiV1ArkhamInvestigatorsR :: Handler [CardDef 'InvestigatorType]
getApiV1ArkhamInvestigatorsR = pure $ toList allInvestigatorCards
