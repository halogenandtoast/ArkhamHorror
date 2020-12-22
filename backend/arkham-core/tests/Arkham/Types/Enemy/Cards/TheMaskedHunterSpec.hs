module Arkham.Types.Enemy.Cards.TheMaskedHunterSpec
  ( spec
  )
where

import TestImport

spec :: Spec
spec = describe "The Masked Hunter" $ do
  context "modifiers" $ do
    it "prevents engaged investigators from discovering or spending clues" $ do
      theMaskedHunter <- buildEnemy "01121b"
      investigator <- testInvestigator "00000" id
      game <- runGameTest
        investigator
        [engageEnemy investigator theMaskedHunter]
        (enemies %~ insertEntity theMaskedHunter)
      withGame
          game
          (map modifierType
          <$> getModifiersFor TestSource (toTarget investigator) ()
          )
        `shouldReturn` [CannotDiscoverClues, CannotSpendClues]

    it
        "does not prevent unengaged investigators from discovering or spending clues"
      $ do
          theMaskedHunter <- buildEnemy "01121b"
          investigator <- testInvestigator "00000" id
          game <- runGameTest
            investigator
            [ engageEnemy investigator theMaskedHunter
            , disengageEnemy investigator theMaskedHunter
            ]
            (enemies %~ insertEntity theMaskedHunter)
          withGame game (getModifiersFor TestSource (toTarget investigator) ())
            `shouldReturn` []
