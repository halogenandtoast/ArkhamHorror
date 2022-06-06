module Arkham.Classes.HasTokenValue where

import Arkham.InvestigatorId
import Arkham.Token
import Arkham.GameEnv

class HasTokenValue a where
  getTokenValue :: InvestigatorId -> TokenFace -> a -> GameT TokenValue
