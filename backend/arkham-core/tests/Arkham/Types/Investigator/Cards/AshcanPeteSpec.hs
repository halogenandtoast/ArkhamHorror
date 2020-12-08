module Arkham.Types.Investigator.Cards.AshcanPeteSpec
  ( spec
  )
where

import TestImport

spec :: Spec
spec = describe "\"Ashcan\" Pete" $ do
  it "starts with Duke in play" $ do
    let ashcanPete = lookupInvestigator "02005"
    duke <- buildPlayerCard "02014"
    placeholders <- replicateM 5 (buildPlayerCard "01088") -- need to fill deck for setup

    game <- runGameTest
      ashcanPete
      [loadDeck ashcanPete (duke : placeholders), SetupInvestigators]
      id
    updated game ashcanPete `shouldSatisfy` hasCardInPlay game (PlayerCard duke)
  context "Ability" $ do
    it "allows to discard to ready an asset" $ do
      let ashcanPete = lookupInvestigator "02005"
      asset <- testAsset id
      card <- testPlayerCard id
      game <-
        runGameTest
          ashcanPete
          [ loadDeck ashcanPete [card]
          , drawCards ashcanPete 1
          , playAsset ashcanPete asset
          , Exhaust (toTarget asset)
          , CheckWindow (toId ashcanPete) [FastPlayerWindow]
          ]
          (assets %~ insertEntity asset)
        >>= runGameTestOptionMatching
              "activate ability"
              (\case
                Run{} -> True
                _ -> False
              )
        >>= runGameTestOnlyOption "discard card"
        >>= runGameTestOnlyOption "ready asset"
      updated game asset `shouldSatisfy` isReady
  context "Elder Sign" $ do
    it "gives +2 and readies duke" $ do
      let ashcanPete = lookupInvestigator "02005"
      duke <- buildAsset "02014"
      (didPassTest, logger) <- didPassSkillTestBy ashcanPete 2
      game <-
        runGameTest
          ashcanPete
          [ SetTokens [ElderSign]
          , Exhaust (toTarget duke)
          , beginSkillTest ashcanPete SkillIntellect 2
          ]
          (assets %~ insertEntity duke)
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOnlyOptionWithLogger "apply results" logger
      updated game duke `shouldSatisfy` isReady
      readIORef didPassTest `shouldReturn` True
