module Arkham.Helpers.SkillTest where

import Arkham.Classes.HasGame
import Arkham.Prelude
import Arkham.Target

getSkillTestDifficulty :: HasGame m => m (Maybe Int)
getSkillTestTarget :: HasGame m => m (Maybe Target)
