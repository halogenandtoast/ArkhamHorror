module Arkham.Types.Investigator.Cards.WendyAdamsSpec
  ( spec
  )
where

import TestImport

spec :: Spec
spec = describe "Wendy Adams" $ do
  context "ability" $ do
    it "allows you to discard a card to redraw a chaos token" $ do
      let wendyAdams = lookupInvestigator "01005"
      scenario' <- testScenario "0000" id
      card <- testPlayerCard id
      game <-
        runGameTest
          wendyAdams
          [ SetTokens [MinusOne]
          , AddToHand (getInvestigatorId wendyAdams) (PlayerCard card)
          , beginSkillTest wendyAdams SkillWillpower 3
          ]
          (scenario ?~ scenario')
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
        >>= runGameTestOnlyOption "discard card"
        >>= runGameTestOnlyOption "apply results"
      wendyAdams `shouldSatisfy` hasPassedSkillTestBy 0 game TestTarget
  context "elder sign" $ do
    it "gives +0" $ do
      let wendyAdams = lookupInvestigator "01005"
      scenario' <- testScenario "0000" id
      game <-
        runGameTest
          wendyAdams
          [SetTokens [ElderSign], beginSkillTest wendyAdams SkillWillpower 4]
          (scenario ?~ scenario')
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOnlyOption "apply results"
      wendyAdams `shouldSatisfy` hasPassedSkillTestBy 0 game TestTarget
    it "automatically succeeds if Wendy's Amulet is in play" $ do
      let wendyAdams = lookupInvestigator "01005"
      wendysAmulet <- buildAsset "01014"
      scenario' <- testScenario "0000" id
      game <-
        runGameTest
          wendyAdams
          [ SetTokens [ElderSign]
          , playAsset wendyAdams wendysAmulet
          , beginSkillTest wendyAdams SkillWillpower 20
          ]
          ((scenario ?~ scenario') . (assets %~ insertEntity wendysAmulet))
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOnlyOption "apply results"
      wendyAdams `shouldSatisfy` hasPassedSkillTestBy 4 game TestTarget
