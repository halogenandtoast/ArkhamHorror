module Arkham.Types.Treachery.Cards.WrackedByNightmaresSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Asset.Attrs as Asset

spec :: Spec
spec = describe "Wracked by Nightmares" $ do
  it "prevents controlled assets from readying" $ do
    investigator <- testInvestigator "00000" id
    wrackedByNightmares <- buildPlayerCard "02015"
    asset <- testAsset
      ((Asset.exhaustedL .~ True)
      . (Asset.investigatorL ?~ getInvestigatorId investigator)
      )
    game <- runGameTest
      investigator
      [ loadDeck investigator [wrackedByNightmares]
      , drawCards investigator 1
      , ReadyExhausted
      ]
      (assets %~ insertEntity asset)
    updated game investigator `shouldSatisfy` hasTreacheryWithMatchingCardCode
      game
      (PlayerCard wrackedByNightmares)
    updated game asset `shouldSatisfy` isExhausted

  it "trigger actions removes restriction and takes two actions" $ do
    investigator <- testInvestigator "00000" id
    wrackedByNightmares <- buildPlayerCard "02015"
    asset <- testAsset
      ((Asset.exhaustedL .~ True)
      . (Asset.investigatorL ?~ getInvestigatorId investigator)
      )
    game <- runGameTest
      investigator
      [loadDeck investigator [wrackedByNightmares], drawCards investigator 1]
      (assets %~ insertEntity asset)
    let wrackedByNightmaresTreachery = game ^?! treacheries . to toList . ix 0
    [discardWrackedByNightmares] <- getActionsOf
      game
      investigator
      NonFast
      wrackedByNightmaresTreachery
    game' <- runGameTestMessages
      game
      [discardWrackedByNightmares, ReadyExhausted]
    updated game' investigator
      `shouldSatisfy` not
      . hasTreacheryWithMatchingCardCode game (PlayerCard wrackedByNightmares)
    updated game' asset `shouldSatisfy` isReady
    wrackedByNightmares `shouldSatisfy` isInDiscardOf game' investigator
    investigator `shouldSatisfy` hasRemainingActions game' 1

