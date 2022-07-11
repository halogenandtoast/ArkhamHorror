module Arkham.Investigator.Cards.WendyAdamsSpec
  ( spec
  ) where

import TestImport.Lifted

spec :: Spec
spec = describe "Wendy Adams" $ do
  context "ability" $ do
    it "allows you to discard a card to redraw a chaos token" $ do
      let wendyAdams = lookupInvestigator "01005"
      card <- testPlayerCard id

      (didPassTest, logger) <- didPassSkillTestBy wendyAdams SkillWillpower 0

      gameTestWithLogger
          logger
          wendyAdams
          [ SetTokens [MinusOne]
          , AddToHand (toId wendyAdams) (PlayerCard card)
          , beginSkillTest wendyAdams SkillWillpower 3
          ]
          id
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
            chooseOnlyOption "discard card"
            chooseOnlyOption "apply results"
            didPassTest `refShouldBe` True

  context "elder sign" $ do
    it "gives +0" $ do
      let wendyAdams = lookupInvestigator "01005"

      (didPassTest, logger) <- didPassSkillTestBy wendyAdams SkillWillpower 0

      gameTestWithLogger
          logger
          wendyAdams
          [SetTokens [ElderSign], beginSkillTest wendyAdams SkillWillpower 4]
          id
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"
            didPassTest `refShouldBe` True

    it "automatically succeeds if Wendy's Amulet is in play" $ do
      let wendyAdams = lookupInvestigator "01005"
      wendysAmulet <- buildAsset "01014" (Just wendyAdams)

      (didPassTest, logger) <- didPassSkillTestBy wendyAdams SkillWillpower 4

      gameTestWithLogger
          logger
          wendyAdams
          [ SetTokens [ElderSign]
          , playAsset wendyAdams wendysAmulet
          , beginSkillTest wendyAdams SkillWillpower 20
          ]
          (entitiesL . assetsL %~ insertEntity wendysAmulet)
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"
            didPassTest `refShouldBe` True
