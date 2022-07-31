module Arkham.Helpers.SkillTest where

import Arkham.Prelude

import Arkham.Action
import Arkham.Classes.Entity
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.SkillTest.Base
import Arkham.Source
import Arkham.Target

getSkillTestTarget :: (Monad m, HasGame m) => m (Maybe Target)
getSkillTestTarget = fmap skillTestTarget <$> getSkillTest

getSkillTestSource :: (Monad m, HasGame m) => m (Maybe Source)
getSkillTestSource = fmap toSource <$> getSkillTest

getSkillTestAction :: (Monad m, HasGame m) => m (Maybe Action)
getSkillTestAction = getSkillTestSource <&> \case
  Just (SkillTestSource _ _ _ maction) -> maction
  _ -> Nothing
