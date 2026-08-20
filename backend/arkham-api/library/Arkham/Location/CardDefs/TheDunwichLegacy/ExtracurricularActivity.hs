module Arkham.Location.CardDefs.TheDunwichLegacy.ExtracurricularActivity where

import Arkham.Location.CardDefs.Import

administrationBuilding :: CardDef
administrationBuilding =
  location
    "02053"
    "Administration Building"
    [Miskatonic]
    Circle
    [Plus, T]
    ExtracurricularActivity

alchemyLabs :: CardDef
alchemyLabs =
  location
    "02057"
    "Alchemy Labs"
    [Miskatonic]
    Squiggle
    [Hourglass]
    ExtracurricularActivity

dormitories :: CardDef
dormitories =
  victory 1
    $ location
      "02052"
      "Dormitories"
      [Miskatonic]
      Equals
      [Diamond]
      ExtracurricularActivity

facultyOfficesTheHourIsLate :: CardDef
facultyOfficesTheHourIsLate =
  location
    "02055"
    ("Faculty Offices" <:> "The Hour is Late")
    [Miskatonic]
    T
    [Circle]
    ExtracurricularActivity

facultyOfficesTheNightIsStillYoung :: CardDef
facultyOfficesTheNightIsStillYoung =
  victory 1
    $ location
      "02054"
      ("Faculty Offices" <:> "The Night is Still Young")
      [Miskatonic]
      T
      [Circle]
      ExtracurricularActivity

humanitiesBuilding :: CardDef
humanitiesBuilding =
  location
    "02049"
    "Humanities Building"
    [Miskatonic]
    Square
    [Plus, Triangle]
    ExtracurricularActivity

miskatonicQuad :: CardDef
miskatonicQuad =
  location
    "02048"
    "Miskatonic Quad"
    [Miskatonic]
    Plus
    [Triangle, Hourglass, Square, Diamond, Circle]
    ExtracurricularActivity

orneLibrary :: CardDef
orneLibrary =
  victory 1
    $ location
      "02050"
      "Orne Library"
      [Miskatonic]
      Triangle
      [Plus, Square]
      ExtracurricularActivity

scienceBuilding :: CardDef
scienceBuilding =
  location
    "02056"
    "Science Building"
    [Miskatonic]
    Hourglass
    [Plus, Squiggle]
    ExtracurricularActivity

studentUnion :: CardDef
studentUnion =
  location
    "02051"
    "Student Union"
    [Miskatonic]
    Diamond
    [Plus, Equals]
    ExtracurricularActivity
