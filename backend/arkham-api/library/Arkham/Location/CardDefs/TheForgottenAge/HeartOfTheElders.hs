module Arkham.Location.CardDefs.TheForgottenAge.HeartOfTheElders where

import Arkham.Location.CardDefs.Import

chthonianDepths :: CardDef
chthonianDepths =
  victory 1
    $ singleSided
    $ location
      "53051"
      "Chthonian Depths"
      [Ancient, Cave]
      Square
      [Heart, Triangle, Circle]
      ReturnToKnYan

crystalPillars :: CardDef
crystalPillars =
  victory 1
    $ singleSided
    $ location
      "04226"
      "Crystal Pillars"
      [Ancient, Cave]
      Moon
      [Heart, Diamond, Circle]
      KnYan

darkHollow :: CardDef
darkHollow =
  victory 1
    $ singleSided
    $ location
      "04224"
      "Dark Hollow"
      [Ancient, Cave]
      Triangle
      [Equals, Circle, Square]
      KnYan

descentToYoth :: CardDef
descentToYoth =
  victory 2
    $ vengeance 2
    $ singleSided
    $ location
      "04227"
      "Descent to Yoth"
      [Ancient, Cave]
      Heart
      [Square, Moon]
      KnYan

hallOfIdolatry :: CardDef
hallOfIdolatry =
  victory 1
    $ singleSided
    $ location
      "04223"
      "Hall of Idolatry"
      [Ancient, Cave]
      Square
      [Heart, Triangle, Circle]
      KnYan

mouthOfKnYanTheCavernsMaw :: CardDef
mouthOfKnYanTheCavernsMaw =
  otherSideIs "04206b"
    $ location
      "04206"
      ("Mouth of K'n-yan" <:> "The Cavern's Maw")
      [Cave]
      Equals
      [Squiggle, T, Hourglass]
      HeartOfTheElders

mouthOfKnYanTheDepthsBeneath :: CardDef
mouthOfKnYanTheDepthsBeneath =
  otherSideIs "04206"
    $ location
      "04206b"
      ("Mouth of K'n-yan" <:> "The Depths Beneath")
      [Cave]
      Equals
      [Circle, Triangle, Diamond]
      HeartOfTheElders

perilousGulch :: CardDef
perilousGulch =
  victory 1
    $ singleSided
    $ location
      "04225"
      "Perilous Gulch"
      [Ancient, Cave]
      Diamond
      [Equals, Circle, Moon]
      KnYan

ruinsOfKnYan :: CardDef
ruinsOfKnYan =
  victory 1
    $ singleSided
    $ location
      "53049"
      "Ruins of K'n-Yan"
      [Ancient, Cave, Ruins]
      Triangle
      [Equals, Circle, Square]
      ReturnToKnYan

stoneAltar :: CardDef
stoneAltar =
  victory 1
    $ singleSided
    $ location
      "04218"
      "Stone Altar"
      [Ancient, Ruins]
      Hourglass
      [Triangle, Heart, Equals]
      PillarsOfJudgement

subterraneanSwamp :: CardDef
subterraneanSwamp =
  victory 1
    $ singleSided
    $ location
      "53050"
      "Subterranean Swamp"
      [Ancient, Cave]
      Diamond
      [Equals, Circle, Moon]
      ReturnToKnYan

timeWrackedWoods :: CardDef
timeWrackedWoods =
  victory 1
    $ singleSided
    $ location
      "04217"
      "Time-Wracked Woods"
      [Jungle]
      Circle
      [Square, Diamond, Moon]
      PillarsOfJudgement

treacherousDescent :: CardDef
treacherousDescent =
  victory 1
    $ singleSided
    $ location
      "53052"
      "Treacherous Descent"
      [Ancient, Cave]
      Moon
      [Heart, Diamond, Circle]
      ReturnToKnYan

vastPassages :: CardDef
vastPassages =
  singleSided
    $ location
      "04222"
      "Vast Passages"
      [Ancient, Cave]
      Circle
      [Equals, Triangle, Diamond, Square, Moon]
      KnYan
