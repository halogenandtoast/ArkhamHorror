module Arkham.UltimatumsAndBoons.BoonOfAthenaSpec (spec) where

import Arkham.ChaosBag.Base
import Arkham.Helpers.Scenario
import Arkham.Scenario.Types (Field (..))
import Helpers.UltimatumsAndBoons
import TestImport.New

-- The bag holds only the autofail so the reveal (and the redraw after the
-- cancel) are deterministic; the redraw revealing the autofail again also
-- proves the reaction is limited to once per game (no second cancel offer).
spec :: Spec
spec = describe "Boon of Athena" $ do
  it "cancels a revealed autofail and draws another token (once per game)" . gameTest $ \self -> do
    withUltimatumsAndBoons [BoonOfAthena]
    setChaosTokens [AutoFail]
    drewAnother <- createMessageChecker \case
      DrawAnotherChaosToken _ -> True
      _ -> False
    sid <- getRandom
    run $ beginSkillTest sid self SkillIntellect 0
    startSkillTest
    useReactionOf (UltimatumOrBoonSource (Boon BoonOfAthena))
    drewAnother `refShouldBe` True
    -- the redrawn autofail resolves without a second cancel offer
    applyResults
    tokens <- scenarioFieldMap ScenarioChaosBag (map chaosTokenFace . chaosBagChaosTokens)
    tokens `shouldMatchList` [AutoFail]

  it "returns the cancelled autofail to the bag" . gameTest $ \self -> do
    withUltimatumsAndBoons [BoonOfAthena]
    setChaosTokens [AutoFail]
    returnedAutoFail <- createMessageChecker \case
      ReturnChaosTokens [token] -> chaosTokenFace token == AutoFail
      _ -> False
    sid <- getRandom
    run $ beginSkillTest sid self SkillIntellect 0
    startSkillTest
    useReactionOf (UltimatumOrBoonSource (Boon BoonOfAthena))
    returnedAutoFail `refShouldBe` True
