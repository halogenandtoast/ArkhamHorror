module Arkham.Types.Asset.Cards.ArcaneInitiateSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Card.PlayerCard as PlayerCard
import Arkham.Types.Trait

spec :: Spec
spec = describe "Arcane Initiate" $ do
  it "enters play with 1 doom" $ do
    arcaneInitiate <- buildAsset "01063"
    investigator <- testInvestigator "00000" id
    game <- runGameTest
      investigator
      [playAsset investigator arcaneInitiate]
      (assets %~ insertEntity arcaneInitiate)
    getCount () (updated game arcaneInitiate) `shouldBe` DoomCount 1
  it "can be exhausted to search the top 3 cards of your deck for a Spell card"
    $ do
        arcaneInitiate <- buildAsset "01063"
        investigator <- testInvestigator "00000" id
        card <- testPlayerCard $ set PlayerCard.traits (setFromList [Spell])
        otherCards <- testPlayerCards 2
        game <- runGameTest
          investigator
          [ playAsset investigator arcaneInitiate
          , loadDeck investigator (card : otherCards)
          ]
          (assets %~ insertEntity arcaneInitiate)

        [ability] <- getActionsOf game investigator NonFast arcaneInitiate

        game' <-
          runGameTestMessages game [ability]
          >>= runGameTestOnlyOption "search top of deck"
          >>= runGameTestOnlyOption "take spell card"
        updated game' investigator `shouldSatisfy` handIs [PlayerCard card]
  it "should continue if no Spell card is found" $ do
    arcaneInitiate <- buildAsset "01063"
    investigator <- testInvestigator "00000" id
    cards <- testPlayerCards 3
    game <- runGameTest
      investigator
      [playAsset investigator arcaneInitiate, loadDeck investigator cards]
      (assets %~ insertEntity arcaneInitiate)

    [ability] <- getActionsOf game investigator NonFast arcaneInitiate

    game' <-
      runGameTestMessages game [ability]
      >>= runGameTestOnlyOption "search top of deck"
      >>= runGameTestOptionMatching
            "no cards found"
            (\case
              Continue{} -> True
              _ -> False
            )
    updated game' investigator `shouldSatisfy` handIs []
