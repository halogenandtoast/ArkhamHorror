module Arkham.Story.CardsSpec (spec) where

import Arkham.Story.Cards qualified as Stories
import TestImport qualified as TI
import TestImport.New

spec :: Spec
spec = describe "Story card definitions" do
  it "links printed story backs to their asset or location sides" do
    map (.otherSide)
      [ Stories.engramsOath
      , Stories.songsThatTheHyadesShallSing
      , Stories.starsOfAldebaran
      , Stories.bleakDesolation
      , Stories.inhabitantOfCarcosa
      , Stories.aMomentsRest
      , Stories.theCoffin
      , Stories.mappingTheStreets
      , Stories.theKingsParade
      , Stories.theArchway
      , Stories.theHeightOfTheDepths
      , Stories.stepsOfThePalace
      ]
      `TI.shouldBe` map
        Just
        [ "03076a"
        , "03325a"
        , "03326a"
        , "03326c"
        , "03327a"
        , "03327c"
        , "03327e"
        , "03328a"
        , "03328c"
        , "03328e"
        , "03329a"
        , "03329c"
        ]
