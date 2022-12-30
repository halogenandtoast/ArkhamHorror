module Arkham.Enemy.Cards.TheMaskedHunterSpec
  ( spec
  ) where

import TestImport.Lifted

spec :: Spec
spec = describe "The Masked Hunter" $ do
  context "modifiers" $ do
    it "prevents engaged investigators from discovering or spending clues" $ do
      theMaskedHunter <- buildEnemy "01121b"
      investigator <- testJenny id
      location <- testLocation id
      gameTest
          investigator
          [ placedLocation location
          , moveTo investigator location
          , engageEnemy investigator theMaskedHunter
          ]
          ((entitiesL . enemiesL %~ insertEntity theMaskedHunter)
          .(entitiesL . locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            getModifiers (toTarget investigator)
              `shouldReturn` [CannotDiscoverClues, CannotSpendClues]

    it
        "does not prevent unengaged investigators from discovering or spending clues"
      $ do
          theMaskedHunter <- buildEnemy "01121b"
          investigator <- testJenny id
          location <- testLocation id
          gameTest
              investigator
              [ placedLocation location
              , moveTo investigator location
              , engageEnemy investigator theMaskedHunter
              , disengageEnemy investigator theMaskedHunter
              ]
              ((entitiesL . enemiesL %~ insertEntity theMaskedHunter)
              .(entitiesL . locationsL %~ insertEntity location))
            $ do
                runMessages
                getModifiers (toTarget investigator)
                  `shouldReturn` []
