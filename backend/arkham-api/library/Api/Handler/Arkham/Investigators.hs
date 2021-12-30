module Api.Handler.Arkham.Investigators
  ( getApiV1ArkhamInvestigatorsR
  ) where

import Import

import Arkham.Prelude (With(..), with)
import Arkham.Investigator
import Arkham.ModifierData

getApiV1ArkhamInvestigatorsR :: Handler [With Investigator ConnectionData]
getApiV1ArkhamInvestigatorsR =
  pure $ map (`with` ConnectionData []) $ toList allInvestigators
