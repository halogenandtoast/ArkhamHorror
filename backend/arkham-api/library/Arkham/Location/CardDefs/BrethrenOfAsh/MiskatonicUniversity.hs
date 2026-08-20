module Arkham.Location.CardDefs.BrethrenOfAsh.MiskatonicUniversity where

import Arkham.Location.CardDefs.Import

dormitories :: CardDef
dormitories =
  location
    "12117"
    "Dormitories"
    [Miskatonic]
    Triangle
    [Spade, Plus]
    MiskatonicUniversity

miskatonicQuad :: CardDef
miskatonicQuad =
  location
    "12116"
    "Miskatonic Quad"
    [Miskatonic, Central]
    Plus
    [Triangle, Hourglass, Circle, Heart, Square, Diamond]
    MiskatonicUniversity

orneLibrary :: CardDef
orneLibrary =
  victory 1
    $ location
      "12120"
      "Orne Library"
      [Miskatonic]
      Diamond
      [Plus]
      MiskatonicUniversity

scienceHall :: CardDef
scienceHall =
  victory 1
    $ location
      "12118"
      "Science Hall"
      [Miskatonic]
      Hourglass
      [Plus]
      MiskatonicUniversity

warrenObservatory :: CardDef
warrenObservatory =
  location
    "12119"
    "Warren Observatory"
    [Miskatonic]
    Square
    [Plus]
    MiskatonicUniversity
