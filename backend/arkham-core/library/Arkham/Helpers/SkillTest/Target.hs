module Arkham.Helpers.SkillTest.Target where

import Arkham.Classes.HasGame
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Prelude
import Arkham.SkillTest.Base
import Arkham.Target

getSkillTestTarget :: HasGame m => m (Maybe Target)
getSkillTestTarget = fmap skillTestTarget <$> getSkillTest

withSkillTestTarget :: HasGame m => (Target -> m ()) -> m ()
withSkillTestTarget f = getSkillTestTarget >>= traverse_ f
