module Arkham.Location.CardDefs.ReturnToTheDunwichLegacy.ReturnToBloodOnTheAltar where

import Arkham.Location.CardDefs.Import

returnToBishopsBrook :: CardDef
returnToBishopsBrook =
  location
    "51034"
    "Bishop's Brook"
    [Dunwich]
    Square
    [Plus, Circle, Triangle]
    ReturnToBloodOnTheAltar

returnToBurnedRuins :: CardDef
returnToBurnedRuins =
  location
    "51035"
    "Burned Ruins"
    [Dunwich]
    Triangle
    [Square, Diamond]
    ReturnToBloodOnTheAltar

returnToCongregationalChurch :: CardDef
returnToCongregationalChurch =
  location
    "51037"
    "Congregational Church"
    [Dunwich]
    Diamond
    [Plus, Triangle, Squiggle]
    ReturnToBloodOnTheAltar

returnToHouseInTheReeds :: CardDef
returnToHouseInTheReeds =
  location
    "51038"
    "House in the Reeds"
    [Dunwich]
    Squiggle
    [Diamond, Moon]
    ReturnToBloodOnTheAltar

returnToOsbornsGeneralStore :: CardDef
returnToOsbornsGeneralStore =
  location
    "51036"
    "Osborn's General Store"
    [Dunwich]
    Circle
    [Moon, Square]
    ReturnToBloodOnTheAltar

returnToSchoolhouse :: CardDef
returnToSchoolhouse =
  location
    "51039"
    "Schoolhouse"
    [Dunwich]
    Moon
    [Plus, Squiggle, Circle]
    ReturnToBloodOnTheAltar

villageCommonsSilentDecay :: CardDef
villageCommonsSilentDecay =
  location
    "51033"
    ("Village Commons" <:> "Silent Decay")
    [Dunwich, Central]
    Plus
    [Square, Diamond, Moon]
    ReturnToBloodOnTheAltar
