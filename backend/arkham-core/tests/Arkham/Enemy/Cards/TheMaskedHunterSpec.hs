module Arkham.Enemy.Cards.TheMaskedHunterSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Enemy.Cards qualified as Enemies

spec :: Spec
spec = describe "The Masked Hunter" $ do
  context "modifiers" $ do
    it "prevents engaged investigators from discovering or spending clues" $ gameTest $ \investigator -> do
      theMaskedHunter <- testEnemyWithDef Enemies.theMaskedHunter id
      location <- testLocationWith id
      pushAndRun $ placedLocation location
      pushAndRun $ moveTo investigator location
      pushAndRun $ engageEnemy investigator theMaskedHunter
      getModifiers investigator `shouldContainM` [CannotDiscoverClues, CannotSpendClues]

    it
      "does not prevent unengaged investigators from discovering or spending clues"
      $ gameTest
      $ \investigator -> do
        theMaskedHunter <- testEnemyWithDef Enemies.theMaskedHunter id
        location <- testLocationWith id
        pushAndRun $ placedLocation location
        pushAndRun $ moveTo investigator location
        pushAndRun $ engageEnemy investigator theMaskedHunter
        pushAndRun $ disengageEnemy investigator theMaskedHunter
        getModifiers investigator `shouldNotContainM` [CannotDiscoverClues, CannotSpendClues]
