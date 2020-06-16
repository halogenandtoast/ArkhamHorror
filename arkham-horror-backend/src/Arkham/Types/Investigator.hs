module Arkham.Types.Investigator where

import ClassyPrelude
import Data.Aeson

data ArkhamInvestigator = ArkhamInvestigator
  { investigatorName :: Text
  , investigatorImage :: Text
  , investigatorPortrait :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

