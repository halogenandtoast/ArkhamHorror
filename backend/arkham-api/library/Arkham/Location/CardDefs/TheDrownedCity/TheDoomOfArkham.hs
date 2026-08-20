module Arkham.Location.CardDefs.TheDrownedCity.TheDoomOfArkham where

import Arkham.Location.CardDefs.Import

downtownRuined :: CardDef
downtownRuined =
  location
    "11693"
    ("Downtown" <:> "Ruined")
    [Arkham, Ruined]
    Triangle
    [Moon, T, Star]
    TheDoomOfArkhamPartII

easternRooftops :: CardDef
easternRooftops =
  location
    "11700"
    "Eastern Rooftops"
    [Arkham, Rooftop]
    Heart
    [Circle, Square, Moon]
    TheDoomOfArkhamPartII

easttownRuined :: CardDef
easttownRuined =
  location
    "11694"
    ("Easttown" <:> "Ruined")
    [Arkham, Ruined]
    Moon
    [Circle, Triangle, Heart]
    TheDoomOfArkhamPartII

miskatonicUniversityRuined :: CardDef
miskatonicUniversityRuined =
  location
    "11695"
    ("Miskatonic University" <:> "Ruined")
    [Arkham, Ruined]
    Diamond
    [T, Plus, Circle, Square, Star]
    TheDoomOfArkhamPartII

northsideRuined :: CardDef
northsideRuined =
  location
    "11692"
    ("Northside" <:> "Ruined")
    [Arkham, Ruined]
    T
    [Diamond, Triangle, Star]
    TheDoomOfArkhamPartII

rivertownRuined :: CardDef
rivertownRuined =
  location
    "11696"
    ("Rivertown" <:> "Ruined")
    [Arkham, Central, Ruined]
    Circle
    [Moon, Diamond, Square, Heart]
    TheDoomOfArkhamPartII

southsideRuined :: CardDef
southsideRuined =
  location
    "11698"
    ("Southside" <:> "Ruined")
    [Arkham, Ruined]
    Square
    [Diamond, Plus, Circle, Heart]
    TheDoomOfArkhamPartII

stMarysHospitalRuined :: CardDef
stMarysHospitalRuined =
  location
    "11697"
    ("St. Mary's Hospital" <:> "Ruined")
    [Arkham, Ruined]
    Plus
    [Diamond, Square, Star]
    TheDoomOfArkhamPartII

tillinghastEsotericaEphemeralShop :: CardDef
tillinghastEsotericaEphemeralShop =
  singleSided
    $ victory 1
    $ location_
      "11685"
      ("Tillinghast Esoterica" <:> "Ephemeral Shop")
      [Sanctum, Extradimensional]
      TheDoomOfArkhamPartI

westernRooftops :: CardDef
westernRooftops =
  location "11699" "Western Rooftops" [Arkham, Rooftop] Star [Diamond, T, Plus] TheDoomOfArkhamPartII
