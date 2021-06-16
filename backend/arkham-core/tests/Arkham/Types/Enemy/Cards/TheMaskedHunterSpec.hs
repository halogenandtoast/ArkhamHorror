module Arkham.Types.Enemy.Cards.TheMaskedHunterSpec
  ( spec
  )
where

import TestImport.Lifted

import Arkham.Types.Modifier

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
            (map modifierType
              <$> getModifiersFor (TestSource mempty) (toTarget investigator) ()
              )
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
                getModifiersFor (TestSource mempty) (toTarget investigator) ()
                  `shouldReturn` []
