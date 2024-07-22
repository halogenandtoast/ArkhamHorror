module Arkham.Event.Cards.WillToSurvive3Spec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Will to Survive (3)" $ do
  it "cancels all tokens for the turn" . gameTest $ \self -> do
    withProp @"intellect" 3 self
    setChaosTokens [AutoFail]
    self `playEvent` Events.willToSurvive3
    sid <- getRandom
    runSkillTest sid self #intellect 3
    assertPassedSkillTest

  it "it is cancelled at the end of the turn" . gameTest $ \self -> do
    withProp @"intellect" 3 self
    setChaosTokens [AutoFail]
    duringTurn self $ self `playEvent` Events.willToSurvive3
    sid <- getRandom
    runSkillTest sid self SkillIntellect 3
    assertFailedSkillTest
