module Arkham.Types.Difficulty
  ( ArkhamDifficulty(..)
  )
where

import ClassyPrelude
import Data.Aeson

data ArkhamDifficulty = ArkhamEasy | ArkhamStandard | ArkhamHard | ArkhamExpert
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
