module Arkham.Types.Investigator.Cards.RexMurphySpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Location.Attrs as Location
import Arkham.Types.Target
import Arkham.Types.Token

spec :: Spec
spec = describe "Rex Murphy" $ do
  context "special ability" $ do
    it "discovers a clue if succeed a skill test by 2 or more" $ do
      let rexMurphy = lookupInvestigator "02002"
      scenario' <- testScenario "00000" id
      location <- testLocation "00000" (Location.clues .~ 1)
      game <-
        runGameTest
          rexMurphy
          [ SetTokens [Zero]
          , moveTo rexMurphy location
          , beginSkillTest rexMurphy SkillIntellect 2
          ]
          ((locations %~ insertEntity location) . (scenario ?~ scenario'))
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
      scenario' <- testScenario "00000" id
      game <-
        runGameTest
          rexMurphy
          [ SetTokens [ElderSign]
          , loadDeck rexMurphy cards
          , BeginSkillTest
            (getId () rexMurphy)
            TestSource
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
      scenario' <- testScenario "00000" id
      game <-
        runGameTest
          rexMurphy
          [ SetTokens [ElderSign]
          , loadDeck rexMurphy cards
          , BeginSkillTest
            (getId () rexMurphy)
            TestSource
            Nothing
            SkillIntellect
            6 -- two higher
            mempty
            mempty
            mempty
            mempty
          ]
          (scenario ?~ scenario')
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOptionMatching
              "resolve normally"
              (\case
                Label "Automatically fail to draw 3" _ -> False
                _ -> True
              )

        >>= runGameTestOnlyOption "apply results"
      updated game rexMurphy `shouldSatisfy` handIs []
      game `shouldSatisfy` hasProcessedMessage
        (PassedSkillTest
          (getId () rexMurphy)
          Nothing
          TestSource
          SkillTestInitiatorTarget
          0
        )
