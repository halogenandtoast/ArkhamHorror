module Arkham.Location.CardDefs.TheScarletKeys.ShadesOfSuffering where

import Arkham.Location.CardDefs.Import

kualaLumpurStationEastWing :: CardDef
kualaLumpurStationEastWing =
  location
    "09667"
    ("Kuala Lumpur Station" <:> "East Wing")
    [KualaLumpur]
    Diamond
    [Moon, Star, Triangle, Squiggle]
    ShadesOfSuffering

kualaLumpurStationWestWing :: CardDef
kualaLumpurStationWestWing =
  victory 1
    $ location
      "09668"
      ("Kuala Lumpur Station" <:> "West Wing")
      [KualaLumpur, Haunted]
      Triangle
      [Diamond, Circle, Square]
      ShadesOfSuffering

melatisShop :: CardDef
melatisShop =
  location
    "09671"
    "Melati's Shop"
    [KualaLumpur]
    Squiggle
    [Diamond]
    ShadesOfSuffering

selangorClub :: CardDef
selangorClub =
  location
    "09669"
    "Selangor Club"
    [KualaLumpur]
    Circle
    [Triangle, Square]
    ShadesOfSuffering

selangorClubPadang :: CardDef
selangorClubPadang =
  victory 1
    $ location
      "09670"
      "Selangor Club Padang"
      [KualaLumpur]
      Square
      [Triangle, Circle]
      ShadesOfSuffering

tinMine :: CardDef
tinMine =
  victory 1
    $ location
      "09673"
      "Tin Mine"
      [KualaLumpur, Haunted]
      Star
      [Diamond]
      ShadesOfSuffering

wayangKulitTheater :: CardDef
wayangKulitTheater =
  victory 1
    $ location
      "09672"
      "Wayang Kulit Theater"
      [KualaLumpur, Haunted]
      Moon
      [Diamond]
      ShadesOfSuffering
