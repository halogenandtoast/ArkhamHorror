module Arkham.Types.Investigator.Cards.RexMurphySpec
  ( spec
  )
where

import TestImport.Lifted

import Arkham.Types.Location.Attrs as Location

spec :: Spec
spec = describe "Rex Murphy" $ do
  context "special ability" $ do
    it "discovers a clue if succeed a skill test by 2 or more" $ do
      let rexMurphy = lookupInvestigator "02002"
      location <- testLocation (Location.cluesL .~ 1)
      gameTest
          rexMurphy
          [ SetTokens [Zero]
          , moveTo rexMurphy location
          , beginSkillTest rexMurphy SkillIntellect 2
          ]
          (locationsL %~ insertEntity location)
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"
            chooseOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
            updated rexMurphy `shouldSatisfyM` hasClueCount 1
            updated location `shouldSatisfyM` hasClueCount 0

  context "elder sign token" $ do
    it "can autofail to draw 3 cards" $ do
      let rexMurphy = lookupInvestigator "02002"
      cards <- testPlayerCards 3
      gameTest
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
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOptionMatching
              "automatically fail"
              (\case
                Label "Automatically fail to draw 3" _ -> True
                _ -> False
              )

            chooseOnlyOption "apply results"
            updated rexMurphy `shouldSatisfyM` handIs (map PlayerCard cards)

    it "can resolve normally with +2" $ do
      let rexMurphy = lookupInvestigator "02002"
      cards <- testPlayerCards 3

      (didPassTest, logger) <- didPassSkillTestBy rexMurphy SkillIntellect 0

      gameTestWithLogger
          logger
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
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOptionMatching
              "resolve normally"
              (\case
                Label "Automatically fail to draw 3" _ -> False
                _ -> True
              )

            chooseOnlyOption "apply results"
            updated rexMurphy `shouldSatisfyM` handIs []
            didPassTest `refShouldBe` True
