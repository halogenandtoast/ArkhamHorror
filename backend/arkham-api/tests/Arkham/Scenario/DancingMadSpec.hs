module Arkham.Scenario.DancingMadSpec (spec) where

import Arkham.Difficulty
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Scenario.Types (Field (..))
import Arkham.Skill.Cards qualified as Skills
import TestImport.New

{- | The Tablet chaos token on Dancing Mad hollows the non-weakness cards
committed to the test. On Easy/Standard this only happens "if you fail"; on
Hard/Expert it is unconditional and immediate: the cards are set aside the
moment the token is revealed, which removes them from the test. See issue #5091.
-}
spec :: Spec
spec = describe "Dancing Mad" $ do
  context "Tablet chaos token" $ do
    it "sets committed non-weakness cards aside immediately on reveal (Hard/Expert)"
      . scenarioTestWithDifficulty Investigators.jennyBarnes Hard "09591"
      $ \self -> do
        courage <- genCard Skills.unexpectedCourage
        self `addToHand` courage
        withProp @"willpower" 10 self
        setChaosTokens [Tablet]
        sid <- getRandom
        run $ beginSkillTest sid self #willpower 1
        commit courage
        startSkillTest
        -- Set aside as a hollow the instant the token resolves, before results
        -- are applied (so its icons no longer count toward the test).
        setAsideMid <- map toCardId <$> scenarioField ScenarioSetAsideCards
        setAsideMid `shouldContain` [toCardId courage]
        applyResults

    it "does not hollow committed cards on a passed test (Easy/Standard)"
      . scenarioTestWithDifficulty Investigators.jennyBarnes Easy "09591"
      $ \self -> do
        courage <- genCard Skills.unexpectedCourage
        self `addToHand` courage
        withProp @"willpower" 10 self
        setChaosTokens [Tablet]
        sid <- getRandom
        run $ beginSkillTest sid self #willpower 1
        commit courage
        startSkillTest
        applyResults
        setAside <- map toCardId <$> scenarioField ScenarioSetAsideCards
        setAside `shouldNotContain` [toCardId courage]
