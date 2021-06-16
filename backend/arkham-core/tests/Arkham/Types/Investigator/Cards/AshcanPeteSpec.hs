module Arkham.Types.Investigator.Cards.AshcanPeteSpec
  ( spec
  )
where

import TestImport.Lifted

spec :: Spec
spec = describe "\"Ashcan\" Pete" $ do
  it "starts with Duke in play" $ do
    let ashcanPete = lookupInvestigator "02005"
    duke <- buildPlayerCard "02014"
    placeholders <- replicateM 5 (buildPlayerCard "01088") -- need to fill deck for setup

    gameTest
        ashcanPete
        [loadDeck ashcanPete (duke : placeholders), SetupInvestigators]
        id
      $ do
          runMessages
          ashcanPete' <- updated ashcanPete
          hasCardInPlay (PlayerCard duke) ashcanPete' `shouldReturn` True

  context "Ability" $ do
    it "allows to discard to ready an asset" $ do
      let ashcanPete = lookupInvestigator "02005"
      asset <- testAsset id
      card <- testPlayerCard id
      gameTest
          ashcanPete
          [ loadDeck ashcanPete [card]
          , drawCards ashcanPete 1
          , playAsset ashcanPete asset
          , Exhaust (toTarget asset)
          , CheckWindow (toId ashcanPete) [FastPlayerWindow]
          ]
          (assetsL %~ insertEntity asset)
        $ do
            runMessages
            chooseOptionMatching
              "activate ability"
              (\case
                Run{} -> True
                _ -> False
              )
            chooseOnlyOption "discard card"
            chooseOnlyOption "ready asset"
            updated asset `shouldSatisfyM` isReady

  context "Elder Sign" $ do
    it "gives +2 and readies duke" $ do
      let ashcanPete = lookupInvestigator "02005"
      duke <- buildAsset "02014"

      (didPassTest, logger) <- didPassSkillTestBy ashcanPete SkillIntellect 2

      gameTestWithLogger
          logger
          ashcanPete
          [ SetTokens [ElderSign]
          , Exhaust (toTarget duke)
          , beginSkillTest ashcanPete SkillIntellect 2
          ]
          (assetsL %~ insertEntity duke)
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"
            updated duke `shouldSatisfyM` isReady
            didPassTest `refShouldBe` True
