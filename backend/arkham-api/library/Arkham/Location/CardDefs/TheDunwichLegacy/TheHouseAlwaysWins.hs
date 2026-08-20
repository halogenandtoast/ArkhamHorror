module Arkham.Location.CardDefs.TheDunwichLegacy.TheHouseAlwaysWins where

import Arkham.Location.CardDefs.Import

artGallery :: CardDef
artGallery =
  victory 1
    $ locationWithUnrevealed
      "02075"
      "Back Hall Doorway"
      [CloverClub]
      T
      [Diamond]
      "Art Gallery"
      [CloverClub]
      Hourglass
      [Diamond]
      TheHouseAlwaysWins

backAlley :: CardDef
backAlley =
  victory 1
    $ locationWithUnrevealed
      "02077"
      "Back Hall Doorway"
      [CloverClub]
      T
      [Diamond]
      "Back Alley"
      [CloverClub]
      Squiggle
      [Diamond]
      TheHouseAlwaysWins

cloverClubBar :: CardDef
cloverClubBar =
  location
    "02072"
    "Clover Club Bar"
    [CloverClub]
    Square
    [Triangle, Circle]
    TheHouseAlwaysWins

cloverClubCardroom :: CardDef
cloverClubCardroom =
  location
    "02073"
    "Clover Club Cardroom"
    [CloverClub]
    Triangle
    [Circle, Square, Diamond]
    TheHouseAlwaysWins

cloverClubLounge :: CardDef
cloverClubLounge =
  location
    "02071"
    "Clover Club Lounge"
    [CloverClub]
    Circle
    [Moon, Square, Triangle]
    TheHouseAlwaysWins

darkenedHall :: CardDef
darkenedHall =
  location
    "02074"
    "Darkened Hall"
    [CloverClub]
    Diamond
    [Triangle, T, Hourglass, Plus, Squiggle]
    TheHouseAlwaysWins

laBellaLuna :: CardDef
laBellaLuna =
  location "02070" "La Bella Luna" [Arkham] Moon [Circle] TheHouseAlwaysWins

vipArea :: CardDef
vipArea =
  victory 1
    $ locationWithUnrevealed
      "02076"
      "Back Hall Doorway"
      [CloverClub]
      T
      [Diamond]
      "VIP Area"
      [CloverClub]
      Plus
      [Diamond]
      TheHouseAlwaysWins
