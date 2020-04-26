module Arkham.Types.SkillTest where

import Arkham.Types.Action
import Arkham.Types.Location
import Arkham.Types.Simple
import Arkham.Types.Skill
import Data.Aeson.Encoding
import GHC.Generics
import Json
import Prelude (Int, Show, fail, pure, ($))

data ArkhamSkillTestTarget
  = ArkhamSkillTestTargetLocation ArkhamLocation
  | ArkhamSkillTestTargetMythosCard
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "target" ArkhamSkillTestTarget

data ArkhamSkillTestResultType
  = ArkhamSkillTestResultTypeSuccess
  | ArkhamSkillTestResultTypeFailure
  deriving stock (Show)

instance ToJSON ArkhamSkillTestResultType where
  toJSON ArkhamSkillTestResultTypeSuccess = "success"
  toJSON ArkhamSkillTestResultTypeFailure = "failure"
  toEncoding ArkhamSkillTestResultTypeSuccess = text "success"
  toEncoding ArkhamSkillTestResultTypeFailure = text "success"

instance FromJSON ArkhamSkillTestResultType where
  parseJSON = withText "ArkhamSkillTestResultType" $ \case
    "success" -> pure ArkhamSkillTestResultTypeSuccess
    "failure" -> pure ArkhamSkillTestResultTypeFailure
    _ -> fail "Not a valid ArkhamSkillTestResultType"

-- TODO: A skill test can be a part of a card
data ArkhamSkillTest = ArkhamSkillTest
  { arkhamSkillTestBase :: Int
  , arkhamSkillTestSkill :: ArkhamSkill
  , arkhamSkillTestAction :: ArkhamAction
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamSkillTest") ArkhamSkillTest

-- Arbitrary value based on game state
-- on failure
-- on condition
-- another token

-- The Gathering (Easy/Standard)
-- Skull = -X. X is the umber of Ghoul enemies at your location
-- Cultist = -1. If you fail, tke 1 horror.
-- Tablet = -2. If ther eis a ghoul enemy at your location, take 1 damage
--
-- The Gathering (Hard/Expert)
-- Skull = -2. If you fail, after this skill test, searh the encounter deck
--   and discard pile for a Ghoul enemy, and draw it. Shuffle the encounter deck.
-- Cultist = Reveal another token. If you fail, take 2 horror
-- Tablet = -4. If there is a Ghoul enemy at your location, take 1 damage and 1 horror.

data ArkhamSkillTestResult = ArkhamSkillTestResult
  { arkhamSkillTestResultToken :: ArkhamChaosToken
  , arkhamSkillTestResultBase :: Int
  , arkhamSkillTestResultSkill :: ArkhamSkill
  , arkhamSkillTestResultAction :: ArkhamAction
  , arkhamSkillTestResultType :: ArkhamSkillTestResultType
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamSkillTestResult") ArkhamSkillTestResult

