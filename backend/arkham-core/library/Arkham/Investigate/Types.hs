module Arkham.Investigate.Types where

import Arkham.Prelude

import Arkham.Id
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Data.UUID (nil)
import GHC.Records

data Investigate = MkInvestigate
  { investigateInvestigator :: InvestigatorId
  , investigateLocation :: LocationId
  , investigateSkillType :: SkillType
  , investigateSource :: Source
  , investigateTarget :: Maybe Target
  , investigateIsAction :: Bool
  , investigateSkillTest :: SkillTestId
  }
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass ToJSON

instance FromJSON Investigate where
  parseJSON = withObject "Investigate" $ \o -> do
    investigateInvestigator <- o .: "investigateInvestigator"
    investigateLocation <- o .: "investigateLocation"
    investigateSkillType <- o .: "investigateSkillType"
    investigateSource <- o .: "investigateSource"
    investigateTarget <- o .:? "investigateTarget"
    investigateIsAction <- o .: "investigateIsAction"
    investigateSkillTest <- o .:? "investigateSkillTestId" .!= SkillTestId nil
    pure $ MkInvestigate {..}

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

instance HasField "skillTest" Investigate SkillTestId where
  getField = investigateSkillTest

instance WithTarget Investigate where
  getTarget = investigateTarget
  setTarget t i = i {investigateTarget = Just (toTarget t)}
