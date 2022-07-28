module Arkham.Investigator.Cards.RexMurphySpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Action qualified as Action
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Types as Location
import Arkham.Projection

spec :: Spec
spec = describe "Rex Murphy" $ do
  context "special ability" $ do
    it "discovers a clue if succeed a skill test by 2 or more" $ do
      let rexMurphy = lookupInvestigator "02002"
      location1 <- testLocation (Location.cluesL .~ 2)
      gameTest
          rexMurphy
          [ SetTokens [Zero]
          , moveTo rexMurphy location1
          , beginActionSkillTest
            rexMurphy
            Action.Investigate
            (Just $ toTarget location1)
            SkillIntellect
            2
          ]
          (entitiesL . locationsL %~ insertEntity location1)
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
            fieldAssert InvestigatorClues (== 2) rexMurphy
            fieldAssert LocationClues (== 0) location1

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
            field InvestigatorHand (toId rexMurphy)
              `shouldMatchListM` map PlayerCard cards

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
            handIs [] rexMurphy `shouldReturn` True
            didPassTest `refShouldBe` True
