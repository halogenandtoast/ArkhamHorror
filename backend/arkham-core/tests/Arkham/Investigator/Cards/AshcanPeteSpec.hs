module Arkham.Investigator.Cards.AshcanPeteSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Asset.Attrs (Field(..))
import Arkham.Matcher (assetIs)

spec :: Spec
spec = describe "\"Ashcan\" Pete" $ do
  it "starts with Duke in play" $ do
    let ashcanPete = lookupInvestigator "02005"
    duke <- genPlayerCard Assets.duke
    placeholders <- replicateM 5 (genPlayerCard Events.emergencyCache) -- need to fill deck for setup

    gameTest
        ashcanPete
        [loadDeck ashcanPete (duke : placeholders), SetupInvestigators]
        id
      $ do
          runMessages
          selectAny (assetIs Assets.duke) `shouldReturn` True

  context "Ability" $ do
    it "allows to discard to ready an asset" $ do
      let ashcanPete = lookupInvestigator "02005"
      asset <- testAsset id ashcanPete
      card <- testPlayerCard id
      gameTest
          ashcanPete
          [ loadDeck ashcanPete [card]
          , drawCards ashcanPete 1
          , playAsset ashcanPete asset
          , Exhaust (toTarget asset)
          , CheckWindow [toId ashcanPete] [fastPlayerWindow]
          ]
          (entitiesL . assetsL %~ insertEntity asset)
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
            fieldAssert AssetExhausted (== False) asset

  context "Elder Sign" $ do
    it "gives +2 and readies duke" $ do
      let ashcanPete = lookupInvestigator "02005"
      duke <- buildAsset "02014" (Just ashcanPete)

      (didPassTest, logger) <- didPassSkillTestBy ashcanPete SkillIntellect 2

      gameTestWithLogger
          logger
          ashcanPete
          [ SetTokens [ElderSign]
          , Exhaust (toTarget duke)
          , beginSkillTest ashcanPete SkillIntellect 2
          ]
          (entitiesL . assetsL %~ insertEntity duke)
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"
            fieldAssert AssetExhausted (== False) duke
            didPassTest `refShouldBe` True
