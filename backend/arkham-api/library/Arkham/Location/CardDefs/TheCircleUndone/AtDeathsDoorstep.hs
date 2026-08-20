module Arkham.Location.CardDefs.TheCircleUndone.AtDeathsDoorstep where

import Arkham.Location.CardDefs.Import

balcony :: CardDef
balcony =
  location
    "05076"
    "Balcony"
    []
    Moon
    [Heart]
    AtDeathsDoorstep

balconySpectral :: CardDef
balconySpectral =
  location
    "05083"
    "Balcony"
    [Spectral]
    Moon
    [Heart]
    AtDeathsDoorstep

billiardsRoom :: CardDef
billiardsRoom =
  location
    "05074"
    "Billiards Room"
    []
    Diamond
    [Triangle]
    AtDeathsDoorstep

billiardsRoomSpectral :: CardDef
billiardsRoomSpectral =
  victory 1
    $ location
      "05081"
      "Billiards Room"
      [Spectral]
      Diamond
      [Triangle]
      AtDeathsDoorstep

entryHall :: CardDef
entryHall =
  location
    "05071"
    "Entry Hall"
    []
    Square
    [T]
    AtDeathsDoorstep

entryHallSpectral :: CardDef
entryHallSpectral =
  location
    "05078"
    "Entry Hall"
    [Spectral]
    Square
    [T]
    AtDeathsDoorstep

masterBedroom :: CardDef
masterBedroom =
  location
    "05075"
    "Master Bedroom"
    []
    Heart
    [T, Moon]
    AtDeathsDoorstep

masterBedroomSpectral :: CardDef
masterBedroomSpectral =
  victory 1
    $ location
      "05082"
      "Master Bedroom"
      [Spectral]
      Heart
      [T, Moon]
      AtDeathsDoorstep

office :: CardDef
office =
  location
    "05077"
    "Office"
    []
    Star
    [T]
    AtDeathsDoorstep

officeSpectral :: CardDef
officeSpectral =
  victory 2
    $ location
      "05084"
      "Office"
      [Spectral]
      Star
      [T]
      AtDeathsDoorstep

trophyRoom :: CardDef
trophyRoom =
  location
    "05073"
    "Trophy Room"
    []
    Triangle
    [T, Diamond]
    AtDeathsDoorstep

trophyRoomSpectral :: CardDef
trophyRoomSpectral =
  location
    "05080"
    "Trophy Room"
    [Spectral]
    Triangle
    [T, Diamond]
    AtDeathsDoorstep

victorianHalls :: CardDef
victorianHalls =
  location
    "05072"
    "Victorian Halls"
    []
    T
    [Square, Star, Triangle, Heart]
    AtDeathsDoorstep

victorianHallsSpectral :: CardDef
victorianHallsSpectral =
  location
    "05079"
    "Victorian Halls"
    [Spectral]
    T
    [Square, Star, Triangle, Heart]
    AtDeathsDoorstep
