module Arkham.Types.Asset.Cards.ArcaneInitiateSpec
  ( spec
  )
where

import TestImport.Lifted

import qualified Arkham.Types.Card.PlayerCard as PlayerCard
import Arkham.Types.Trait

spec :: Spec
spec = describe "Arcane Initiate" $ do
  it "enters play with 1 doom" $ do
    arcaneInitiate <- buildAsset "01063"
    investigator <- testInvestigator "00000" id
    runGameTest
        investigator
        [playAsset investigator arcaneInitiate]
        (assetsL %~ insertEntity arcaneInitiate)
      $ do
          runMessagesNoLogging
          doomCount <- getCount =<< updated arcaneInitiate
          doomCount `shouldBe` DoomCount 1

  it "can be exhausted to search the top 3 cards of your deck for a Spell card"
    $ do
        arcaneInitiate <- buildAsset "01063"
        investigator <- testInvestigator "00000" id
        card <- testPlayerCard $ set PlayerCard.traitsL (setFromList [Spell])
        otherCards <- testPlayerCards 2
        runGameTest
            investigator
            [ playAsset investigator arcaneInitiate
            , loadDeck investigator (card : otherCards)
            ]
            (assetsL %~ insertEntity arcaneInitiate)
          $ do
              runMessagesNoLogging
              [ability] <- getActionsOf
                investigator
                FastPlayerWindow
                arcaneInitiate
              runGameTestMessages [ability]
              runGameTestOnlyOption "search top of deck"
              runGameTestOnlyOption "take spell card"
              updated investigator `shouldSatisfyM` handIs [PlayerCard card]

  it "should continue if no Spell card is found" $ do
    arcaneInitiate <- buildAsset "01063"
    investigator <- testInvestigator "00000" id
    cards <- testPlayerCards 3
    runGameTest
        investigator
        [playAsset investigator arcaneInitiate, loadDeck investigator cards]
        (assetsL %~ insertEntity arcaneInitiate)
      $ do
          runMessagesNoLogging
          [ability] <- getActionsOf investigator FastPlayerWindow arcaneInitiate
          runGameTestMessages [ability]
          runGameTestOnlyOption "search top of deck"
          runGameTestOptionMatching
            "no cards found"
            (\case
              Label{} -> True
              _ -> False
            )
          updated investigator `shouldSatisfyM` handIs []
