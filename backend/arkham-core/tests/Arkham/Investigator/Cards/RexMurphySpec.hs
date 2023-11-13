module Arkham.Investigator.Cards.RexMurphySpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Action qualified as Action
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Types as Location
import Arkham.Projection
import Arkham.SkillTest.Base

spec :: Spec
spec = describe "Rex Murphy" $ do
  context "special ability" $ do
    it "discovers a clue if succeed a skill test by 2 or more" $ gameTestWith Investigators.rexMurphy $ \rexMurphy -> do
      location1 <- testLocationWith (Location.revealCluesL .~ Static 2)
      pushAndRunAll
        [ SetChaosTokens [Zero]
        , moveTo rexMurphy location1
        , beginActionSkillTest
            rexMurphy
            Action.Investigate
            (Just $ toTarget location1)
            SkillIntellect
            2
        ]
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"
      chooseOptionMatching
        "use ability"
        ( \case
            AbilityLabel {} -> True
            _ -> False
        )
      fieldAssert InvestigatorClues (== 2) rexMurphy
      fieldAssert LocationClues (== 0) location1

  context "elder sign token" $ do
    it "can autofail to draw 3 cards" $ gameTestWith Investigators.rexMurphy $ \rexMurphy -> do
      cards <- testPlayerCards 3
      pushAndRunAll
        [ SetChaosTokens [ElderSign]
        , loadDeck rexMurphy cards
        , BeginSkillTest
            $ initSkillTest
              (toId rexMurphy)
              (TestSource mempty)
              TestTarget
              SkillIntellect
              2
        ]
      chooseOnlyOption "start skill test"
      chooseOptionMatching
        "automatically fail"
        ( \case
            Label "Automatically fail to draw 3" _ -> True
            _ -> False
        )

      chooseOnlyOption "apply results"
      field InvestigatorHand (toId rexMurphy)
        `shouldMatchListM` map PlayerCard cards

    it "can resolve normally with +2" $ gameTestWith Investigators.rexMurphy $ \rexMurphy -> do
      cards <- testPlayerCards 3

      didPassTest <- didPassSkillTestBy rexMurphy SkillIntellect 0

      pushAndRunAll
        [ SetChaosTokens [ElderSign]
        , loadDeck rexMurphy cards
        , BeginSkillTest
            $ initSkillTest
              (toId rexMurphy)
              (TestSource mempty)
              TestTarget
              SkillIntellect
              6 -- two higher
        ]
      chooseOnlyOption "start skill test"
      chooseOptionMatching
        "resolve normally"
        ( \case
            Label "Automatically fail to draw 3" _ -> False
            _ -> True
        )

      chooseOnlyOption "apply results"
      assert $ handIs [] rexMurphy
      didPassTest `refShouldBe` True
