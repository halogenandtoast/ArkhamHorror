module Api.Handler.Arkham.Investigators (
  getApiV1ArkhamInvestigatorsR,
) where

import Import

import Arkham.Card.CardDef
import Arkham.Investigator.Cards

getApiV1ArkhamInvestigatorsR :: Handler [Text]
getApiV1ArkhamInvestigatorsR = pure $ map cdArt $ toList allInvestigatorCards
