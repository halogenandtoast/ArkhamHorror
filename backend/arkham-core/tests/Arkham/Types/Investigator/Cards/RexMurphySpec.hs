module Arkham.Types.Investigator.Cards.RexMurphySpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Location.Attrs as Location

spec :: Spec
spec = describe "Rex Murphy" $ do
  context "special ability" $ do
    it "discovers a clue if succeed a skill test by 2 or more" $ do
      let rexMurphy = lookupInvestigator "02002"
      location <- testLocation "00000" (Location.cluesL .~ 1)
      game <-
        runGameTest
          rexMurphy
          [ SetTokens [Zero]
          , moveTo rexMurphy location
          , beginSkillTest rexMurphy SkillIntellect 2
          ]
          (locationsL %~ insertEntity location)
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOnlyOption "apply results"
        >>= runGameTestOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
      updated game rexMurphy `shouldSatisfy` hasClueCount 1
      updated game location `shouldSatisfy` hasClueCount 0

  context "elder sign token" $ do
    it "can autofail to draw 3 cards" $ do
      let rexMurphy = lookupInvestigator "02002"
      cards <- testPlayerCards 3
      game <-
        runGameTest
          rexMurphy
          [ SetTokens [ElderSign]
          , loadDeck rexMurphy cards
          , BeginSkillTest
            (toId rexMurphy)
            (TestSource mempty)
            TestTarget
            Nothing
            SkillIntellect
            2
          ]
          id
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOptionMatching
              "automatically fail"
              (\case
                Label "Automatically fail to draw 3" _ -> True
                _ -> False
              )

        >>= runGameTestOnlyOption "apply results"
      updated game rexMurphy `shouldSatisfy` handIs (map PlayerCard cards)

    it "can resolve normally with +2" $ do
      let rexMurphy = lookupInvestigator "02002"
      cards <- testPlayerCards 3
      (didPassTest, logger) <- didPassSkillTestBy rexMurphy 0
      game <-
        runGameTest
          rexMurphy
          [ SetTokens [ElderSign]
          , loadDeck rexMurphy cards
          , BeginSkillTest
            (toId rexMurphy)
            (TestSource mempty)
            TestTarget
            Nothing
            SkillIntellect
            6 -- two higher
          ]
          id
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOptionMatching
              "resolve normally"
              (\case
                Label "Automatically fail to draw 3" _ -> False
                _ -> True
              )

        >>= runGameTestOnlyOptionWithLogger "apply results" logger
      updated game rexMurphy `shouldSatisfy` handIs []
      readIORef didPassTest `shouldReturn` True
