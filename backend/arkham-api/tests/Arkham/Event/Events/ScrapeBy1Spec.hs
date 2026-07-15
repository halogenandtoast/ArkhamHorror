module Arkham.Event.Events.ScrapeBy1Spec (spec) where

import Arkham.Event.Cards qualified as Events
import Helpers.Message qualified
import TestImport.New

spec :: Spec
spec = describe "Scrape By (1)" do
  it "succeeds by 0, not by your skill value" . gameTest $ \self -> do
    withProp @"intellect" 5 self
    withProp @"resources" 1 self
    scrapeBy1 <- genCard Events.scrapeBy1
    self `addToHand` scrapeBy1

    didPassBy0 <- didPassSkillTestBy self #intellect 0

    setChaosTokens [Zero]
    sid <- getRandom
    -- Scrape By is committable, so the fast window offers more than "start"
    run $ Helpers.Message.beginSkillTest sid self #intellect 8
    startSkillTest
    assertFailedSkillTest
    chooseTarget scrapeBy1
    assertPassedSkillTest
    click "Apply results"
    didPassBy0 `refShouldBe` True

  it "succeeds even when the investigator fails ties (The 13th Vision)" . gameTest $ \self -> do
    withProp @"intellect" 5 self
    withProp @"resources" 1 self
    scrapeBy1 <- genCard Events.scrapeBy1
    self `addToHand` scrapeBy1

    didPassBy0 <- didPassSkillTestBy self #intellect 0

    setChaosTokens [Zero]
    sid <- getRandom
    run =<< gameModifier (TestSource mempty) (SkillTestTarget sid) FailTies
    -- a tie, so FailTies is the only reason this is a failure to begin with
    run $ Helpers.Message.beginSkillTest sid self #intellect 5
    startSkillTest
    assertFailedSkillTest
    chooseTarget scrapeBy1
    assertPassedSkillTest
    click "Apply results"
    didPassBy0 `refShouldBe` True
