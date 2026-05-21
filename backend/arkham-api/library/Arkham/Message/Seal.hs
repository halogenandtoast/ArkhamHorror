{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Seal where

import Arkham.ChaosToken.Types
import Arkham.Id
import Arkham.Prelude
import Arkham.Target
import Data.Aeson.TH

-- | Messages dealing with chaos token sealing on game entities.
data SealMessage
  = SealChaosToken_ ChaosToken
  | SealedChaosToken_ ChaosToken (Maybe InvestigatorId) Target
  | SetChaosTokenAside_ ChaosToken
  | UnsealChaosToken_ ChaosToken
  | RemoveAllChaosTokens_ ChaosTokenFace
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''SealMessage)
