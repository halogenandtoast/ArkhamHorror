module Arkham.Types.Investigator.Cards.JimCulverSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Target
import Arkham.Types.Token

spec :: Spec
spec = describe "Jum Culver" $ do
  context "elder sign" $ do
    it "can be changed to a skull" $ do
      let jimCulver = lookupInvestigator "02004"
      scenario' <- testScenario "00000" id
      game <-
        runGameTest
          jimCulver
          [ SetTokens [ElderSign]
          , BeginSkillTest
            (getId () jimCulver)
            TestSource
            TestTarget
            Nothing
            SkillIntellect
            2
            mempty
            mempty
            mempty
            mempty
          ]
          (scenario ?~ scenario')
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOptionMatching
              "change to skull"
              (\case
                Label "Resolve as Skull" _ -> True
                _ -> False
              )
        >>= runGameTestOnlyOption "apply results"
      game `shouldSatisfy` hasProcessedMessage
        (RunSkillTest (getId () jimCulver) [TokenValue Skull 0])

    it "is a +1" $ do
      let jimCulver = lookupInvestigator "02004"
      scenario' <- testScenario "00000" id
      game <-
        runGameTest
          jimCulver
          [ SetTokens [ElderSign]
          , BeginSkillTest
            (getId () jimCulver)
            TestSource
            TestTarget
            Nothing
            SkillIntellect
            2
            mempty
            mempty
            mempty
            mempty
          ]
          (scenario ?~ scenario')
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOptionMatching
              "resolve elder sign"
              (\case
                Label "Resolve as Elder Sign" _ -> True
                _ -> False
              )
        >>= runGameTestOnlyOption "apply results"
      game `shouldSatisfy` hasProcessedMessage
        (RunSkillTest (getId () jimCulver) [TokenValue ElderSign 1])

  context "ability" $ do
    it "changes skull modifier to 0" $ do
      let jimCulver = lookupInvestigator "02004"
      scenario' <- testScenario "00000" id
      game <-
        runGameTest
          jimCulver
          [ SetTokens [Skull]
          , BeginSkillTest
            (getId () jimCulver)
            TestSource
            TestTarget
            Nothing
            SkillIntellect
            2
            mempty
            mempty
            mempty
            mempty
          ]
          (scenario ?~ scenario')
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOnlyOption "apply results"
      game `shouldSatisfy` hasProcessedMessage
        (RunSkillTest (getId () jimCulver) [TokenValue Skull 0])
