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
      card <- testPlayerCard id
      (didPassTest, logger) <- didPassSkillTestBy wendyAdams 0
      void
        $ runGameTest
            wendyAdams
            [ SetTokens [MinusOne]
            , AddToHand (toId wendyAdams) (PlayerCard card)
            , beginSkillTest wendyAdams SkillWillpower 3
            ]
            id
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
        >>= runGameTestOnlyOption "discard card"
        >>= runGameTestOnlyOptionWithLogger "apply results" logger
      readIORef didPassTest `shouldReturn` True

  context "elder sign" $ do
    it "gives +0" $ do
      let wendyAdams = lookupInvestigator "01005"
      (didPassTest, logger) <- didPassSkillTestBy wendyAdams 0
      void
        $ runGameTest
            wendyAdams
            [SetTokens [ElderSign], beginSkillTest wendyAdams SkillWillpower 4]
            id
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOnlyOptionWithLogger "apply results" logger
      readIORef didPassTest `shouldReturn` True

    it "automatically succeeds if Wendy's Amulet is in play" $ do
      let wendyAdams = lookupInvestigator "01005"
      wendysAmulet <- buildAsset "01014"
      (didPassTest, logger) <- didPassSkillTestBy wendyAdams 4
      void
        $ runGameTest
            wendyAdams
            [ SetTokens [ElderSign]
            , playAsset wendyAdams wendysAmulet
            , beginSkillTest wendyAdams SkillWillpower 20
            ]
            (assetsL %~ insertEntity wendysAmulet)
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOnlyOptionWithLogger "apply results" logger
      readIORef didPassTest `shouldReturn` True
