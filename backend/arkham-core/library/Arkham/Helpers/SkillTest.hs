module Arkham.Helpers.SkillTest where

import Arkham.Prelude

import Arkham.Classes.Entity
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.SkillTest.Base
import Arkham.Source
import Arkham.Target

getSkillTestTarget :: GameT (Maybe Target)
getSkillTestTarget = fmap skillTestTarget <$> getSkillTest

getSkillTestSource :: GameT (Maybe Source)
getSkillTestSource = fmap toSource <$> getSkillTest

