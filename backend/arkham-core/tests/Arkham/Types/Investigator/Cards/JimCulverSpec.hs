module Arkham.Types.Investigator.Cards.JimCulverSpec
  ( spec
  )
where

import TestImport

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
      void
        $ runGameTest
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
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOptionMatchingWithLogger
              "change to skull"
              logger
              (\case
                Label "Resolve as Skull" _ -> True
                _ -> False
              )
        >>= runGameTestOnlyOption "apply results"
      readIORef didResolveSkull `shouldReturn` True

    it "is a +1" $ do
      let jimCulver = lookupInvestigator "02004"
      (didPassTest, logger) <- didPassSkillTestBy jimCulver 2
      void
        $ runGameTest
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
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOptionMatching
              "resolve elder sign"
              (\case
                Label "Resolve as Elder Sign" _ -> True
                _ -> False
              )
        >>= runGameTestOnlyOptionWithLogger "apply results" logger
      readIORef didPassTest `shouldReturn` True

  context "ability" $ do
    it "changes skull modifier to 0" $ do
      let jimCulver = lookupInvestigator "02004"
      (didPassTest, logger) <- didPassSkillTestBy jimCulver 1
      void
        $ runGameTest
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
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOnlyOptionWithLogger "apply results" logger
      readIORef didPassTest `shouldReturn` True
