module Arkham.Enemy.Cards.TheMaskedHunterSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Modifier

spec :: Spec
spec = describe "The Masked Hunter" $ do
  context "modifiers" $ do
    it "prevents engaged investigators from discovering or spending clues" $ do
      theMaskedHunter <- buildEnemy "01121b"
      investigator <- testInvestigator "00000" id
      gameTest
          investigator
          [engageEnemy investigator theMaskedHunter]
          (enemiesL %~ insertEntity theMaskedHunter)
        $ do
            runMessages
            getModifiers (TestSource mempty) (toTarget investigator)
              `shouldReturn` [CannotDiscoverClues, CannotSpendClues]

    it
        "does not prevent unengaged investigators from discovering or spending clues"
      $ do
          theMaskedHunter <- buildEnemy "01121b"
          investigator <- testInvestigator "00000" id
          gameTest
              investigator
              [ engageEnemy investigator theMaskedHunter
              , disengageEnemy investigator theMaskedHunter
              ]
              (enemiesL %~ insertEntity theMaskedHunter)
            $ do
                runMessages
                getModifiers (TestSource mempty) (toTarget investigator)
                  `shouldReturn` []
