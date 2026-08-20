{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.BrethrenOfAsh.Arkham where

import Arkham.EncounterSet qualified as Set
import Arkham.Location.CardDefs.Import

downtownArkhamSanatorium :: CardDef
downtownArkhamSanatorium =
  victory 1
    $ location
      "12146"
      ("Downtown" <:> "Arkham Sanatorium")
      [Arkham]
      Triangle
      [Moon, T]
      Set.Arkham

downtownFirstBankOfArkham_Arkham :: CardDef
downtownFirstBankOfArkham_Arkham =
  victory 1
    $ location
      "12145"
      ("Downtown" <:> "First Bank of Arkham")
      [Arkham]
      Triangle
      [Moon, T]
      Set.Arkham

easttown :: CardDef
easttown =
  location
    "12150"
    "Easttown"
    [Arkham]
    Moon
    [Circle, Triangle, Spade]
    Set.Arkham

frenchHill :: CardDef
frenchHill =
  victory 1
    $ location
      "12154"
      "French Hill"
      [Arkham]
      Hourglass
      [Square]
      Set.Arkham

merchantDistrict :: CardDef
merchantDistrict =
  location
    "12151"
    "Merchant District"
    [Arkham]
    Circle
    [Moon, Diamond, Square, Spade]
    Set.Arkham

miskatonicUniversityInFlames :: CardDef
miskatonicUniversityInFlames =
  victory 1
    $ location
      "12155"
      ("Miskatonic University" <:> "In Flames")
      [Arkham]
      Diamond
      [T, Plus, Circle]
      Set.Arkham

miskatonicUniversityQuietCampus :: CardDef
miskatonicUniversityQuietCampus =
  victory 1
    $ location
      "12156"
      ("Miskatonic University" <:> "Quiet Campus")
      [Arkham]
      Diamond
      [T, Plus, Circle]
      Set.Arkham

northside :: CardDef
northside =
  location
    "12149"
    "Northside"
    [Arkham]
    T
    [Diamond, Triangle]
    Set.Arkham

southside :: CardDef
southside =
  location
    "12153"
    "Southside"
    [Arkham]
    Square
    [Plus, Hourglass, Circle]
    Set.Arkham

uptownStMarysHospital :: CardDef
uptownStMarysHospital =
  location
    "12147"
    ("Uptown" <:> "St. Mary's Hospital")
    [Arkham]
    Plus
    [Diamond, Square]
    Set.Arkham

uptownYeOldeMagickShoppe :: CardDef
uptownYeOldeMagickShoppe =
  location
    "12148"
    ("Uptown" <:> "Ye Olde Magick Shoppe")
    [Arkham]
    Plus
    [Diamond, Square]
    Set.Arkham

waterfrontDistrict :: CardDef
waterfrontDistrict =
  victory 1
    $ location
      "12152"
      "Waterfront District"
      [Arkham]
      Spade
      [Circle, Moon]
      Set.Arkham
