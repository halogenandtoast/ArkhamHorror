module Arkham.Investigator.Cards.JimCulverSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Calculation
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.SkillTest.Base

spec :: Spec
spec = describe "Jim Culver" $ do
  context "elder sign" $ do
    it "can be changed to a skull" $ gameTestWith Investigators.jimCulver $ \jimCulver -> do
      didResolveSkull <- createMessageChecker $ \case
        ResolveChaosToken _ token _ -> token == Skull
        _ -> False
      sid <- getRandom
      pushAndRunAll
        [ SetChaosTokens [ElderSign]
        , BeginSkillTest
            $ initSkillTest
              sid
              (toId jimCulver)
              (TestSource mempty)
              TestTarget
              SkillIntellect
              (SkillTestDifficulty $ Fixed 2)
        ]
      chooseOnlyOption "start skill test"
      chooseOptionMatching
        "change to skull"
        ( \case
            Label "Resolve as {skull}" _ -> True
            _ -> False
        )
      chooseOnlyOption "apply results"
      didResolveSkull `refShouldBe` True

    it "is a +1" $ gameTestWith Investigators.jimCulver $ \jimCulver -> do
      didPassTest <- didPassSkillTestBy jimCulver SkillIntellect 2
      sid <- getRandom
      pushAndRunAll
        [ SetChaosTokens [ElderSign]
        , BeginSkillTest
            $ initSkillTest
              sid
              (toId jimCulver)
              (TestSource mempty)
              TestTarget
              SkillIntellect
              (SkillTestDifficulty $ Fixed 2)
        ]
      chooseOnlyOption "start skill test"
      chooseOptionMatching
        "resolve elder sign"
        ( \case
            Label "Resolve as {elderSign}" _ -> True
            _ -> False
        )
      chooseOnlyOption "apply results"
      didPassTest `refShouldBe` True

  context "ability" $ do
    it "changes skull modifier to 0" $ gameTestWith Investigators.jimCulver $ \jimCulver -> do
      didPassTest <- didPassSkillTestBy jimCulver SkillIntellect 1
      sid <- getRandom
      pushAndRunAll
        [ SetChaosTokens [Skull]
        , BeginSkillTest
            $ initSkillTest
              sid
              (toId jimCulver)
              (TestSource mempty)
              TestTarget
              SkillIntellect
              (SkillTestDifficulty $ Fixed 2)
        ]
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"
      didPassTest `refShouldBe` True
