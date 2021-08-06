module Api.Handler.Arkham.Investigators
  ( getApiV1ArkhamInvestigatorsR
  ) where

import Import

import Arkham.Types.Investigator

getApiV1ArkhamInvestigatorsR :: Handler [Investigator]
getApiV1ArkhamInvestigatorsR = pure $ toList allInvestigators
