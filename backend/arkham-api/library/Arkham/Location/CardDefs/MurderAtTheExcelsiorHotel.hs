module Arkham.Location.CardDefs.MurderAtTheExcelsiorHotel where

import Arkham.Location.CardDefs.Import

basement :: CardDef
basement =
  victory 1
    $ location
      "84019"
      "Basement"
      [CrimeScene]
      Hourglass
      [T, Moon]
      MurderAtTheExcelsiorHotel

foyer :: CardDef
foyer =
  location
    "84013"
    "Foyer"
    [Hall]
    T
    [Square, Squiggle, Hourglass, Moon]
    MurderAtTheExcelsiorHotel

hotelRoof :: CardDef
hotelRoof =
  victory 1
    $ location
      "84015"
      "Hotel Roof"
      []
      Plus
      [Square]
      MurderAtTheExcelsiorHotel

office :: CardDef
office =
  victory 1
    $ location
      "84018"
      "Office"
      []
      Moon
      [T, Hourglass]
      MurderAtTheExcelsiorHotel

restaurant :: CardDef
restaurant =
  location
    "84014"
    "Restaurant"
    [Hall]
    Squiggle
    [Square, T]
    MurderAtTheExcelsiorHotel

room212 :: CardDef
room212 =
  victory 1
    $ location
      "84016"
      "Room 212"
      [CrimeScene]
      Diamond
      [Square]
      MurderAtTheExcelsiorHotel

room225 :: CardDef
room225 =
  location
    "84010"
    ("Room 225" <:> "Scene of the Crime")
    [CrimeScene]
    Circle
    [Square, Triangle]
    MurderAtTheExcelsiorHotel

room245 :: CardDef
room245 =
  victory 1
    $ location
      "84017"
      "Room 245"
      [CrimeScene]
      Equals
      [Square]
      MurderAtTheExcelsiorHotel

secondFloorHall :: CardDef
secondFloorHall =
  location
    "84012"
    "Second Floor Hall"
    [Hall]
    Square
    [Circle, T, Squiggle, Plus, Diamond, Equals]
    MurderAtTheExcelsiorHotel

suiteBalcony :: CardDef
suiteBalcony =
  location
    "84011"
    "Suite Balcony"
    [CrimeScene]
    Triangle
    [Circle]
    MurderAtTheExcelsiorHotel
