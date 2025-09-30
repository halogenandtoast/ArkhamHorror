{-# LANGUAGE TemplateHaskell #-}

module Arkham.Evade.Types where

import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH
import GHC.Records
import Arkham.Question
import {-# SOURCE #-} Arkham.Message

data ChooseEvade = ChooseEvade
  { chooseEvadeInvestigator :: InvestigatorId
  , chooseEvadeEnemyMatcher :: EnemyMatcher
  , chooseEvadeSource :: Source
  , chooseEvadeTarget :: Maybe Target
  , chooseEvadeSkillType :: SkillType
  , chooseEvadeIsAction :: Bool
  , chooseEvadeOverride :: Bool
  , chooseEvadeSkillTest :: SkillTestId
  , chooseEvadeAdditionalOptions :: [UI Message]
  }
  deriving stock (Show, Ord, Eq, Data)

instance HasField "investigator" ChooseEvade InvestigatorId where
  getField = chooseEvadeInvestigator

instance HasField "isAction" ChooseEvade Bool where
  getField = chooseEvadeIsAction

instance HasField "skillType" ChooseEvade SkillType where
  getField = chooseEvadeSkillType

instance HasField "source" ChooseEvade Source where
  getField = chooseEvadeSource

instance HasField "target" ChooseEvade (Maybe Target) where
  getField = chooseEvadeTarget

instance HasField "matcher" ChooseEvade EnemyMatcher where
  getField = chooseEvadeEnemyMatcher

instance HasField "overriden" ChooseEvade Bool where
  getField = chooseEvadeOverride

instance HasField "skillTest" ChooseEvade SkillTestId where
  getField = chooseEvadeSkillTest

instance HasField "additionalOptions" ChooseEvade [UI Message] where
  getField = chooseEvadeAdditionalOptions

instance WithTarget ChooseEvade where
  getTarget = chooseEvadeTarget
  setTarget t i = i {chooseEvadeTarget = Just (toTarget t)}

$(deriveToJSON defaultOptions ''ChooseEvade)

instance FromJSON ChooseEvade where
  parseJSON = withObject "ChooseEvade" \o -> do
    chooseEvadeInvestigator <- o .: "chooseEvadeInvestigator"
    chooseEvadeEnemyMatcher <- o .: "chooseEvadeEnemyMatcher"
    chooseEvadeSource <- o .: "chooseEvadeSource"
    chooseEvadeTarget <- o .:? "chooseEvadeTarget"
    chooseEvadeSkillType <- o .: "chooseEvadeSkillType"
    chooseEvadeIsAction <- o .: "chooseEvadeIsAction"
    chooseEvadeOverride <- o .: "chooseEvadeOverride"
    chooseEvadeSkillTest <- o .: "chooseEvadeSkillTest"
    chooseEvadeAdditionalOptions <- o .:? "chooseEvadeAdditionalOptions" .!= []

    pure ChooseEvade {..}
