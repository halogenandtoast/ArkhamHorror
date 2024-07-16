module Arkham.Investigate.Types where

import Arkham.Prelude

import Arkham.Id
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import GHC.Records

data Investigate = MkInvestigate
  { investigateInvestigator :: InvestigatorId
  , investigateLocation :: LocationId
  , investigateSkillType :: SkillType
  , investigateSource :: Source
  , investigateTarget :: Maybe Target
  , investigateIsAction :: Bool
  }
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "investigator" Investigate InvestigatorId where
  getField = investigateInvestigator

instance HasField "location" Investigate LocationId where
  getField = investigateLocation

instance HasField "isAction" Investigate Bool where
  getField = investigateIsAction

instance HasField "skillType" Investigate SkillType where
  getField = investigateSkillType

instance HasField "source" Investigate Source where
  getField = investigateSource

instance HasField "target" Investigate (Maybe Target) where
  getField = investigateTarget

instance WithTarget Investigate where
  getTarget = investigateTarget
  setTarget t i = i {investigateTarget = Just (toTarget t)}
