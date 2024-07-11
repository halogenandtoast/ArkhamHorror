module Arkham.Evade.Types where

import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import GHC.Records

data ChooseEvade = ChooseEvade
  { chooseEvadeInvestigator :: InvestigatorId
  , chooseEvadeEnemyMatcher :: EnemyMatcher
  , chooseEvadeSource :: Source
  , chooseEvadeTarget :: Maybe Target
  , chooseEvadeSkillType :: SkillType
  , chooseEvadeIsAction :: Bool
  , chooseEvadeOverride :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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

instance WithTarget ChooseEvade where
  getTarget = chooseEvadeTarget
  setTarget t i = i {chooseEvadeTarget = Just (toTarget t)}
