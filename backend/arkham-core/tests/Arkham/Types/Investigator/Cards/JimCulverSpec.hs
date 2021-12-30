module Arkham.Investigator.Cards.JimCulverSpec
  ( spec
  ) where

import TestImport.Lifted

spec :: Spec
spec = describe "Jim Culver" $ do
  context "elder sign" $ do
    it "can be changed to a skull" $ do
      let jimCulver = lookupInvestigator "02004"
      didResolveSkull <- newIORef False
      let
        logger = \case
          ResolveToken _ token _ -> if token == Skull
            then atomicWriteIORef didResolveSkull True
            else pure ()
          _ -> pure ()
      gameTestWithLogger
          logger
          jimCulver
          [ SetTokens [ElderSign]
          , BeginSkillTest
            (toId jimCulver)
            (TestSource mempty)
            TestTarget
            Nothing
            SkillIntellect
            2
          ]
          id
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOptionMatching
              "change to skull"
              (\case
                Label "Resolve as Skull" _ -> True
                _ -> False
              )
            chooseOnlyOption "apply results"
            didResolveSkull `refShouldBe` True

    it "is a +1" $ do
      let jimCulver = lookupInvestigator "02004"

      (didPassTest, logger) <- didPassSkillTestBy jimCulver SkillIntellect 2

      gameTestWithLogger
          logger
          jimCulver
          [ SetTokens [ElderSign]
          , BeginSkillTest
            (toId jimCulver)
            (TestSource mempty)
            TestTarget
            Nothing
            SkillIntellect
            2
          ]
          id
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOptionMatching
              "resolve elder sign"
              (\case
                Label "Resolve as Elder Sign" _ -> True
                _ -> False
              )
            chooseOnlyOption "apply results"
            didPassTest `refShouldBe` True

  context "ability" $ do
    it "changes skull modifier to 0" $ do
      let jimCulver = lookupInvestigator "02004"

      (didPassTest, logger) <- didPassSkillTestBy jimCulver SkillIntellect 1

      gameTestWithLogger
          logger
          jimCulver
          [ SetTokens [Skull]
          , BeginSkillTest
            (toId jimCulver)
            (TestSource mempty)
            TestTarget
            Nothing
            SkillIntellect
            2
          ]
          id
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"
            didPassTest `refShouldBe` True
