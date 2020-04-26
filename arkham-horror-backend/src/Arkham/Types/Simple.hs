module Arkham.Types.Simple where

import Data.Text (pack, unpack)
import Database.Persist.Sql
import GHC.Generics
import Json
import Prelude (Either(..), Show(..), pure, (.))

data ArkhamDifficulty = ArkhamDifficultyEasy | ArkhamDifficultyStandard | ArkhamDifficultyHard | ArkhamDifficultyExpert

instance Show ArkhamDifficulty where
  show ArkhamDifficultyEasy = "easy"
  show ArkhamDifficultyStandard = "standard"
  show ArkhamDifficultyHard = "hard"
  show ArkhamDifficultyExpert = "expert"

instance PersistField ArkhamDifficulty where
  toPersistValue = PersistText . pack . show
  fromPersistValue (PersistText t) = case unpack t of
    "easy" -> pure ArkhamDifficultyEasy
    "standard" -> pure ArkhamDifficultyStandard
    "hard" -> pure ArkhamDifficultyHard
    "expert" -> pure ArkhamDifficultyExpert
    _ -> Left "invalid difficulty"
  fromPersistValue _ = Left "invalid difficulty persist type"

instance PersistFieldSql ArkhamDifficulty where
  sqlType _ = SqlString

data ArkhamChaosToken
  = ArkhamChaosTokenPlusOne
  | ArkhamChaosTokenZero
  | ArkhamChaosTokenMinusOne
  | ArkhamChaosTokenMinusTwo
  | ArkhamChaosTokenMinusThree
  | ArkhamChaosTokenMinusFour
  | ArkhamChaosTokenMinusFive
  | ArkhamChaosTokenMinusSix
  | ArkhamChaosTokenMinusSeven
  | ArkhamChaosTokenMinusEight
  | ArkhamChaosTokenSkull
  | ArkhamChaosTokenCultist
  | ArkhamChaosTokenTablet
  | ArkhamChaosTokenFail
  | ArkhamChaosTokenElderSign
  deriving stock (Generic)
  deriving (ToJSON, FromJSON) via TaggedJson "token" ArkhamChaosToken

instance Show ArkhamChaosToken where
  show ArkhamChaosTokenPlusOne = "+1"
  show ArkhamChaosTokenZero = "0"
  show ArkhamChaosTokenMinusOne = "-1"
  show ArkhamChaosTokenMinusTwo = "-2"
  show ArkhamChaosTokenMinusThree = "-3"
  show ArkhamChaosTokenMinusFour = "-4"
  show ArkhamChaosTokenMinusFive = "-5"
  show ArkhamChaosTokenMinusSix = "-6"
  show ArkhamChaosTokenMinusSeven = "-7"
  show ArkhamChaosTokenMinusEight = "-8"
  show ArkhamChaosTokenSkull = "skull"
  show ArkhamChaosTokenCultist = "cultist"
  show ArkhamChaosTokenTablet = "tablet"
  show ArkhamChaosTokenFail = "fail"
  show ArkhamChaosTokenElderSign = "elderSign"

instance PersistField ArkhamChaosToken where
  toPersistValue = PersistText . pack . show
  fromPersistValue (PersistText t) = case unpack t of
    "+1" -> pure ArkhamChaosTokenPlusOne
    "0" -> pure ArkhamChaosTokenZero
    "-1" -> pure ArkhamChaosTokenMinusOne
    "-2" -> pure ArkhamChaosTokenMinusTwo
    "-3" -> pure ArkhamChaosTokenMinusThree
    "-4" -> pure ArkhamChaosTokenMinusFour
    "-5" -> pure ArkhamChaosTokenMinusFive
    "-6" -> pure ArkhamChaosTokenMinusSix
    "-7" -> pure ArkhamChaosTokenMinusSeven
    "-8" -> pure ArkhamChaosTokenMinusEight
    "skull" -> pure ArkhamChaosTokenSkull
    "cultist" -> pure ArkhamChaosTokenCultist
    "tablet" -> pure ArkhamChaosTokenTablet
    "fail" -> pure ArkhamChaosTokenFail
    "elderSign" -> pure ArkhamChaosTokenElderSign
    _ -> Left "invalid difficulty"
  fromPersistValue _ = Left "invalid difficulty persist type"

instance PersistFieldSql ArkhamChaosToken where
  sqlType _ = SqlString
