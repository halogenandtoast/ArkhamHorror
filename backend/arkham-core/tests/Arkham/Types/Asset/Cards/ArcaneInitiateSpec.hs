module Arkham.Types.Asset.Cards.ArcaneInitiateSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Asset.Cards qualified as Cards

spec :: Spec
spec = describe "Arcane Initiate" $ do
  it "enters play with 1 doom" $ do
    arcaneInitiate <- buildAsset "01063"
    investigator <- testInvestigator "00000" id
    gameTest
        investigator
        [playAsset investigator arcaneInitiate]
        (assetsL %~ insertEntity arcaneInitiate)
      $ do
          runMessages
          chooseOnlyOption "trigger forced ability"
          doomCount <- getCount =<< updated arcaneInitiate
          doomCount `shouldBe` DoomCount 1

  it "can be exhausted to search the top 3 cards of your deck for a Spell card"
    $ do
        arcaneInitiate <- buildAsset "01063"
        investigator <- testInvestigator "00000" id
        card <- genPlayerCard Cards.shrivelling
        otherCards <- testPlayerCards 2
        gameTest
            investigator
            [ loadDeck investigator (card : otherCards)
            , playAsset investigator arcaneInitiate
            ]
            (assetsL %~ insertEntity arcaneInitiate)
          $ do
              runMessages
              chooseOnlyOption "trigger forced ability"
              [_, ability] <- getAbilitiesOf arcaneInitiate
              push $ UseAbility (toId investigator) ability []
              runMessages
              chooseOnlyOption "search top of deck"
              chooseOnlyOption "take spell card"
              updated investigator `shouldSatisfyM` handIs [PlayerCard card]

  it "should continue if no Spell card is found" $ do
    arcaneInitiate <- buildAsset "01063"
    investigator <- testInvestigator "00000" id
    cards <- testPlayerCards 3
    gameTest
        investigator
        [loadDeck investigator cards, playAsset investigator arcaneInitiate]
        (assetsL %~ insertEntity arcaneInitiate)
      $ do
          runMessages
          chooseOnlyOption "trigger forced ability"
          [_, ability] <- getAbilitiesOf arcaneInitiate
          push $ UseAbility (toId investigator) ability []
          runMessages
          chooseOnlyOption "search top of deck"
          chooseOptionMatching
            "no cards found"
            (\case
              Label{} -> True
              _ -> False
            )
          updated investigator `shouldSatisfyM` handIs []
