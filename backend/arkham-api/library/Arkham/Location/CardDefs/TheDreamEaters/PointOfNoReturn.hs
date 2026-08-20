{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheDreamEaters.PointOfNoReturn where

import Arkham.Location.CardDefs.Import

cityOfGugs :: CardDef
cityOfGugs =
  victory 1
    $ otherSideIs "06255b"
    $ location
      "06255"
      "City of Gugs"
      []
      T
      [Heart, Squiggle, Moon]
      PointOfNoReturn

cragOfTheGhouls :: CardDef
cragOfTheGhouls =
  victory 1
    $ veiled
    $ location
      "06258"
      "Crag of the Ghouls"
      [Vale]
      Hourglass
      [Equals, Circle, Moon]
      PointOfNoReturn

peaksOfThok :: CardDef
peaksOfThok =
  otherSideIs "06260b"
    $ location
      "06260"
      "Peaks of Thok"
      [Vale, Central]
      Star
      [Equals, Circle]
      PointOfNoReturn

plainOfTheGhouls :: CardDef
plainOfTheGhouls =
  victory 1
    $ veiled
    $ location
      "06257"
      "Plain of the Ghouls"
      [Central]
      Moon
      [Heart, T, Hourglass]
      PointOfNoReturn

seaOfBones :: CardDef
seaOfBones =
  victory 1
    $ otherSideIs "06259b"
    $ location
      "06259"
      "Sea of Bones"
      [Vale]
      Circle
      [Hourglass, Star, Equals]
      PointOfNoReturn

seaOfPitch_262 :: CardDef
seaOfPitch_262 =
  veiled
    $ location
      "06262"
      "Sea of Pitch"
      [Depths]
      Plus
      [Equals, Plus]
      PointOfNoReturn

seaOfPitch_263 :: CardDef
seaOfPitch_263 =
  veiled
    $ location
      "06263"
      "Sea of Pitch"
      [Depths]
      Plus
      [Equals, Plus]
      PointOfNoReturn

seaOfPitch_264 :: CardDef
seaOfPitch_264 =
  veiled
    $ location
      "06264"
      "Sea of Pitch"
      [Depths]
      Plus
      [Equals, Plus]
      PointOfNoReturn

seaOfPitch_265 :: CardDef
seaOfPitch_265 =
  veiled
    $ location
      "06265"
      "Sea of Pitch"
      [Depths]
      Plus
      [Equals, Plus]
      PointOfNoReturn

towerOfKoth :: CardDef
towerOfKoth =
  otherSideIs "06256b"
    $ location
      "06256"
      "Tower of Koth"
      []
      Squiggle
      [T, Square]
      PointOfNoReturn

valeOfPnath :: CardDef
valeOfPnath =
  victory 1
    $ veiled
    $ location
      "06261"
      "Vale of Pnath"
      [Vale]
      Equals
      [Hourglass, Star, Circle, Plus]
      PointOfNoReturn

vaultsOfZin :: CardDef
vaultsOfZin =
  victory 1
    $ veiled
    $ location
      "06254"
      "Vaults of Zin"
      []
      Heart
      [T, Moon]
      PointOfNoReturn
