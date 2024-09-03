module Arkham.Helpers.SkillTest where

import Arkham.Classes.HasGame
import Arkham.Prelude

getSkillTestDifficulty :: (HasCallStack, HasGame m) => m (Maybe Int)
