module Arkham.Debug.SandboxSpec (spec) where

import Arkham.ChaosBag.Base
import Arkham.Debug.SkillTest
import Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.Scenario
import Arkham.Scenario.Types (Field (..))
import Data.Aeson.KeyMap qualified as KeyMap
import TestImport.New

sandbox :: TestAppT [DebugRevealedChaosToken]
sandbox = scenarioFieldMap ScenarioChaosBag chaosBagDebugRevealedChaosTokens

bagFaces :: TestAppT [ChaosTokenFace]
bagFaces = scenarioFieldMap ScenarioChaosBag (map chaosTokenFace . chaosBagChaosTokens)

sandboxFaces :: TestAppT [ChaosTokenFace]
sandboxFaces = map (chaosTokenFace . debugRevealedToken) <$> sandbox

debugTest :: Investigator -> DebugTestSkill -> DebugSkillTestSpec
debugTest self skill =
  DebugSkillTestSpec
    { debugTestInvestigator = toId self
    , debugTestSkill = skill
    , debugTestAction = Nothing
    , debugTestTarget = Nothing
    , debugTestSourceCard = Nothing
    , debugTestDifficulty = DebugDifficultyFixed 0
    }

loadsBagWithoutSandboxField :: IO ()
loadsBagWithoutSandboxField = case toJSON emptyChaosBag of
  Object o -> do
    let old = Object (KeyMap.delete "debugRevealedChaosTokens" o)
    fmap chaosBagDebugRevealedChaosTokens (decode (encode old) :: Maybe ChaosBag) `shouldBe` Just []
  _ -> expectationFailure "a chaos bag should encode as an object"

spec :: Spec
spec = do
  describe "Debug sandbox reveal" do
    it "draws tokens out of the bag without losing them" . gameTest $ \self -> do
      setChaosTokens [Zero, MinusOne, Skull]
      run $ DebugRevealChaosTokens (toId self) 2
      bag <- scenarioField ScenarioChaosBag
      liftIO do
        length (chaosBagChaosTokens bag) `shouldBe` 1
        length (chaosBagDebugRevealedChaosTokens bag) `shouldBe` 2
        length (allChaosBagChaosTokens bag) `shouldBe` 3

    it "reveals nothing to the game and starts no test" . gameTest $ \self -> do
      setChaosTokens [Skull, Cultist]
      revealed <- createMessageChecker \case
        RevealChaosToken {} -> True
        ResolveChaosToken {} -> True
        RequestChaosTokens {} -> True
        _ -> False
      run $ DebugRevealChaosTokens (toId self) 2
      revealed `refShouldBe` False
      (isJust <$> getSkillTest) `shouldReturn` False

    it "takes the forced face first" . gameTest $ \self -> do
      setChaosTokens [Zero, MinusOne, Skull]
      run $ DebugSetForcedChaosTokenDraws [Skull]
      run $ DebugRevealChaosTokens (toId self) 1
      sandboxFaces `shouldReturn` [Skull]

    it "snapshots the token's value for the calculator" . gameTest $ \self -> do
      setChaosTokens [MinusTwo]
      run $ DebugRevealChaosTokens (toId self) 1
      tokens <- sandbox
      liftIO
        $ map (\t -> (debugRevealedValue t, debugRevealedAutoFail t)) tokens
        `shouldBe` [(Just (-2), False)]

    it "marks an auto-fail as such" . gameTest $ \self -> do
      setChaosTokens [AutoFail]
      run $ DebugRevealChaosTokens (toId self) 1
      tokens <- sandbox
      liftIO $ map debugRevealedAutoFail tokens `shouldBe` [True]

    it "never draws more than the bag holds" . gameTest $ \self -> do
      setChaosTokens [Zero]
      run $ DebugRevealChaosTokens (toId self) 3
      sandboxFaces `shouldReturn` [Zero]
      bagFaces `shouldReturn` []

    it "returns every token to the bag" . gameTest $ \self -> do
      setChaosTokens [Zero, MinusOne, Skull]
      run $ DebugRevealChaosTokens (toId self) 2
      run DebugReturnChaosTokens
      sandbox `shouldReturn` []
      bagFaces `shouldMatchListM` [Zero, MinusOne, Skull]

    it "puts the tokens back before a real skill test starts" . gameTest $ \self -> do
      setChaosTokens [Zero, MinusOne]
      run $ DebugRevealChaosTokens (toId self) 2
      sid <- getRandom
      run $ beginSkillTest sid self SkillIntellect 0
      sandbox `shouldReturn` []
      bagFaces `shouldMatchListM` [Zero, MinusOne]

    it "can seal a sandbox token" . gameTest $ \self -> do
      setChaosTokens [Zero, Skull]
      run $ DebugSetForcedChaosTokenDraws [Skull]
      run $ DebugRevealChaosTokens (toId self) 1
      [token] <- sandbox
      run $ DebugSealChaosToken (chaosTokenId $ debugRevealedToken token) (toTarget self)
      sandbox `shouldReturn` []
      bagFaces `shouldReturn` [Zero]

    it "loads a saved bag from before the sandbox existed" loadsBagWithoutSandboxField

  describe "Debug skill test" do
    it "investigates a location against its shroud" . gameTest $ \self -> do
      withProp @"intellect" 3 self
      location <- testLocation & prop @"shroud" 2 & prop @"clues" 1
      setChaosTokens [Zero]
      self `moveTo` location
      run
        $ DebugBeginSkillTest
          (debugTest self (DebugTestOneSkill SkillIntellect))
            { debugTestAction = Just #investigate
            , debugTestTarget = Just (toTarget location)
            , debugTestDifficulty = DebugDifficultyOfTarget
            }
      startSkillTest
      assertPassedSkillTest
      applyResults
      self.clues `shouldReturn` 1

    it "runs a plain fixed-value test" . gameTest $ \self -> do
      setChaosTokens [Zero]
      run
        $ DebugBeginSkillTest
          (debugTest self (DebugTestFixed 3)) {debugTestDifficulty = DebugDifficultyFixed 2}
      startSkillTest
      assertPassedSkillTest

    it "adds combined skills together" . gameTest $ \self -> do
      withProp @"willpower" 2 self
      withProp @"intellect" 2 self
      setChaosTokens [Zero]
      run
        $ DebugBeginSkillTest
          (debugTest self (DebugTestCombined [SkillWillpower, SkillIntellect]))
            { debugTestDifficulty = DebugDifficultyFixed 4
            }
      startSkillTest
      assertPassedSkillTest

    it "refuses to start inside another skill test" . gameTest $ \self -> do
      setChaosTokens [Zero]
      sid <- getRandom
      run $ beginSkillTest sid self SkillIntellect 0
      run $ DebugBeginSkillTest (debugTest self (DebugTestFixed 0))
      (fmap (.id) <$> getSkillTest) `shouldReturn` Just sid

    it "refuses an investigate test with no location" . gameTest $ \self -> do
      run
        $ DebugBeginSkillTest
          (debugTest self (DebugTestOneSkill SkillIntellect)) {debugTestAction = Just #investigate}
      (isJust <$> getSkillTest) `shouldReturn` False
