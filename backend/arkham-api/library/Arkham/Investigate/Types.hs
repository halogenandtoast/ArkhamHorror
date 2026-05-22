{-# LANGUAGE TemplateHaskell #-}

module Arkham.Investigate.Types where

import Arkham.Id
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH
import GHC.Records

data Investigate = MkInvestigate
  { investigateInvestigator :: InvestigatorId
  , investigateLocation :: LocationId
  , investigateSkillType :: SkillType
  , investigateSource :: Source
  , investigateTarget :: Maybe Target
  , investigateIsAction :: Bool
  , investigatePayCost :: Bool
  , investigateSkillTest :: SkillTestId
  }
  deriving stock (Show, Ord, Eq, Data)

$(deriveToJSON defaultOptions ''Investigate)

instance FromJSON Investigate where
  parseJSON = withObject "Investigate" \o -> do
    investigateInvestigator <- o .: "investigateInvestigator"
    investigateLocation <- o .: "investigateLocation"
    investigateSkillType <- o .: "investigateSkillType"
    investigateSource <- o .: "investigateSource"
    investigateTarget <- o .:? "investigateTarget"
    investigateIsAction <- o .: "investigateIsAction"
    investigatePayCost <- o .:? "investigatePayCost" .!= investigateIsAction
    investigateSkillTest <- o .: "investigateSkillTest"
    pure MkInvestigate {..}

instance HasField "investigator" Investigate InvestigatorId where
  getField = investigateInvestigator

instance HasField "location" Investigate LocationId where
  getField = investigateLocation

instance HasField "isAction" Investigate Bool where
  getField = investigateIsAction

instance HasField "payCost" Investigate Bool where
  getField = investigatePayCost

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
