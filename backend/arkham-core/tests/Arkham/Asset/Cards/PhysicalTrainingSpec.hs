{-# OPTIONS_GHC -Wno-type-defaults #-}

module Arkham.Asset.Cards.PhysicalTrainingSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

default (Int)

spec :: Spec
spec = describe "Physical Training" $ do
  it "Adds 1 to willpower check for each resource spent" $ gameTest $ \self -> do
    withProp @"willpower" 0 self
    withProp @"resources" 2 self
    self `putCardIntoPlay` Assets.physicalTraining
    setChaosTokens [Zero]
    run $ beginSkillTest self #willpower 3
    useFastActionOf Assets.physicalTraining 1
    useFastActionOf Assets.physicalTraining 1
    startSkillTest
    self.skillValue `shouldReturn` 2

  it "Adds 1 to combat check for each resource spent" $ gameTest $ \self -> do
    withProp @"combat" 0 self
    withProp @"resources" 2 self
    self `putCardIntoPlay` Assets.physicalTraining
    setChaosTokens [Zero]
    run $ beginSkillTest self #combat 3
    useFastActionOf Assets.physicalTraining 2
    useFastActionOf Assets.physicalTraining 2
    startSkillTest
    self.skillValue `shouldReturn` 2
