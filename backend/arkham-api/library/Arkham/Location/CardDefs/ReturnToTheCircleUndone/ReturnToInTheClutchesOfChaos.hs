module Arkham.Location.CardDefs.ReturnToTheCircleUndone.ReturnToInTheClutchesOfChaos where

import Arkham.Location.CardDefs.Import

returnToFrenchHill :: CardDef
returnToFrenchHill =
  location
    "54050"
    "French Hill"
    [Arkham]
    T
    [Circle, Square, Star]
    ReturnToInTheClutchesOfChaos

returnToMerchantDistrict :: CardDef
returnToMerchantDistrict =
  location
    "54055"
    "Merchant District"
    [Arkham]
    Triangle
    [Circle, Square, Plus]
    ReturnToInTheClutchesOfChaos

returnToRivertown :: CardDef
returnToRivertown =
  location
    "54051"
    "Rivertown"
    [Arkham]
    Circle
    [Square, Triangle, T]
    ReturnToInTheClutchesOfChaos

returnToSouthChurch :: CardDef
returnToSouthChurch =
  location
    "54054"
    "South Church"
    [Arkham]
    Diamond
    [Square]
    ReturnToInTheClutchesOfChaos

returnToSouthside :: CardDef
returnToSouthside =
  location
    "54052"
    "Southside"
    [Arkham, Central]
    Square
    [Circle, Triangle, Plus, T, Diamond]
    ReturnToInTheClutchesOfChaos

returnToUptown :: CardDef
returnToUptown =
  location
    "54053"
    "Uptown"
    [Arkham]
    Plus
    [Square, Triangle, Moon]
    ReturnToInTheClutchesOfChaos
