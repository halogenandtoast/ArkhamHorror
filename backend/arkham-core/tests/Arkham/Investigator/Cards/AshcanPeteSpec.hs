module Arkham.Investigator.Cards.AshcanPeteSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher (assetIs)

spec :: Spec
spec = describe "\"Ashcan\" Pete" $ do
  it "starts with Duke in play" $ gameTestWith Investigators.ashcanPete $ \ashcanPete -> do
    duke <- genPlayerCard Assets.duke
    placeholders <- replicateM 5 (genPlayerCard Events.emergencyCache) -- need to fill deck for setup
    pushAndRunAll [loadDeck ashcanPete (duke : placeholders), SetupInvestigators]
    assert $ selectAny (assetIs Assets.duke)

  context "Ability" $ do
    it "allows to discard to ready an asset" $ gameTestWith Investigators.ashcanPete $ \ashcanPete -> do
      asset <- testAsset id ashcanPete
      card <- testPlayerCard id
      drawing <- drawCards (toId ashcanPete) ashcanPete 1
      pushAndRunAll
        [ loadDeck ashcanPete [card]
        , drawing
        , playAsset ashcanPete asset
        , Exhaust (toTarget asset)
        , CheckWindow [toId ashcanPete] [fastPlayerWindow]
        ]
      chooseOptionMatching
        "activate ability"
        ( \case
            AbilityLabel {} -> True
            _ -> False
        )
      chooseOnlyOption "discard card"
      chooseOnlyOption "ready asset"
      fieldAssert AssetExhausted (== False) asset

  context "Elder Sign" $ do
    it "gives +2 and readies duke" $ gameTestWith Investigators.ashcanPete $ \ashcanPete -> do
      didPassTest <- didPassSkillTestBy ashcanPete SkillIntellect 2

      putCardIntoPlay ashcanPete Assets.duke
      duke <- selectJust $ assetIs Assets.duke

      pushAndRunAll
        [ SetChaosTokens [ElderSign]
        , Exhaust (toTarget duke)
        , beginSkillTest ashcanPete SkillIntellect 2
        ]
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"
      fieldAssert AssetExhausted (== False) duke
      didPassTest `refShouldBe` True
