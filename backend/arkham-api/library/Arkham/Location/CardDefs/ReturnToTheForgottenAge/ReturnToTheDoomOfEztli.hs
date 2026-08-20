module Arkham.Location.CardDefs.ReturnToTheForgottenAge.ReturnToTheDoomOfEztli where

import Arkham.Location.CardDefs.Import

ancientHallRearrangedByTime :: CardDef
ancientHallRearrangedByTime =
  singleSided
    $ location
      "53025"
      ("Ancient Hall" <:> "Rearranged by Time")
      [Ancient, Ruins]
      Square
      [Circle, Diamond, Heart]
      ReturnToTheDoomOfEztli

chamberOfTimeRearrangedByTime :: CardDef
chamberOfTimeRearrangedByTime =
  victory 2
    $ vengeance 2
    $ singleSided
    $ location
      "53027"
      ("Chamber of Time" <:> "Rearranged by Time")
      [Forgotten, Ruins]
      Hourglass
      [Squiggle, Triangle, T]
      ReturnToTheDoomOfEztli

entrywayRearrangedByTime :: CardDef
entrywayRearrangedByTime =
  location
    "53019"
    ("Entryway" <:> "Rearranged by Time")
    [Ancient, Ruins]
    Circle
    [Square, Diamond, Star]
    ReturnToTheDoomOfEztli

grandChamberRearrangedByTime :: CardDef
grandChamberRearrangedByTime =
  vengeance 1
    $ singleSided
    $ location
      "53026"
      ("Grand Chamber" <:> "Rearranged by Time")
      [Ancient, Ruins]
      Star
      [Circle, Diamond, Plus]
      ReturnToTheDoomOfEztli

mosaicChamber :: CardDef
mosaicChamber =
  singleSided
    $ location
      "53021"
      "Mosaic Chamber"
      [Ancient, Ruins]
      Heart
      [Square, Plus, T, Triangle]
      ReturnToTheDoomOfEztli

sealedPassage :: CardDef
sealedPassage =
  singleSided
    $ location
      "53020"
      "Sealed Passage"
      [Ancient, Ruins]
      Diamond
      [Circle, Square, Star]
      ReturnToTheDoomOfEztli

snakePit :: CardDef
snakePit =
  singleSided
    $ location
      "53024"
      "Snake Pit"
      [Ancient, Ruins]
      T
      [Heart, Plus, Triangle, Hourglass]
      ReturnToTheDoomOfEztli

throneRoom :: CardDef
throneRoom =
  victory 1
    $ vengeance 1
    $ singleSided
    $ location
      "53023"
      "Throne Room"
      [Ancient, Ruins]
      Plus
      [Star, Heart, T, Triangle]
      ReturnToTheDoomOfEztli

tombOfTheAncients :: CardDef
tombOfTheAncients =
  singleSided
    $ location
      "53022"
      "Tomb of the Ancients"
      [Ancient, Ruins]
      Triangle
      [Heart, Plus, T, Hourglass]
      ReturnToTheDoomOfEztli
