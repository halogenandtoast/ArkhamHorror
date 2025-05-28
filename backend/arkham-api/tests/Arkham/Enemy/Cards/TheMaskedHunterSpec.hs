module Arkham.Enemy.Cards.TheMaskedHunterSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import TestImport.Lifted

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
        pushAndRun $ exhaustEnemy theMaskedHunter
        pushAndRun $ disengageEnemy investigator theMaskedHunter
        tick
        getModifiers investigator `shouldNotContainM` [CannotDiscoverClues, CannotSpendClues]
