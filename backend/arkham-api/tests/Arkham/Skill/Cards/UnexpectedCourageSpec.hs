module Arkham.Skill.Cards.UnexpectedCourageSpec (spec) where

import Arkham.Skill.Cards qualified as Skills
import TestImport.New

spec :: Spec
spec = describe "Unexpected Courage" $ do
  maxCommittedPerSkillTest 1 Skills.unexpectedCourage
