module Arkham.Types.TokenPool where

import Arkham.Json
import Arkham.Types.Token
import ClassyPrelude


newtype TokenPool = TokenPool { unTokenPool :: [Token] }
  deriving newtype (Show, ToJSON, FromJSON)

