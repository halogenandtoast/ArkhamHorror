module Arkham.Types.Investigator.Cards.AshcanPeteSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Window

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
    updated game ashcanPete `shouldSatisfy` hasCardInPlay (PlayerCard duke)
  context "Ability" $ do
    it "allows to discard to ready an asset" $ do
      let ashcanPete = lookupInvestigator "02005"
      asset <- testAsset id
      scenario' <- testScenario "00000" id
      card <- testPlayerCard
      game <-
        runGameTest
          ashcanPete
          [ loadDeck ashcanPete [card]
          , drawCards ashcanPete 1
          , playAsset ashcanPete asset
          , Exhaust (AssetTarget $ getId () asset)
          , CheckWindow (getId () ashcanPete) [FastPlayerWindow]
          ]
          ((scenario ?~ scenario') . (assets %~ insertEntity asset))
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
      scenario' <- testScenario "00000" id
      game <-
        runGameTest
          ashcanPete
          [ SetTokens [ElderSign]
          , Exhaust (AssetTarget $ getId () duke)
          , beginSkillTest ashcanPete SkillIntellect 2
          ]
          ((scenario ?~ scenario') . (assets %~ insertEntity duke))
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOnlyOption "apply results"
      updated game duke `shouldSatisfy` isReady
      game `shouldSatisfy` hasProcessedMessage
        (RunSkillTest (getId () ashcanPete) (TokenValue ElderSign 2))
