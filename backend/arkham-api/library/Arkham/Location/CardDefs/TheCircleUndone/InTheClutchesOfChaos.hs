{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheCircleUndone.InTheClutchesOfChaos where

import Arkham.Location.CardDefs.Import

frenchHill_290 :: CardDef
frenchHill_290 =
  location
    "05290"
    "French Hill"
    [Arkham]
    T
    [Circle, Square, Star]
    InTheClutchesOfChaos

frenchHill_291 :: CardDef
frenchHill_291 =
  location
    "05291"
    "French Hill"
    [Arkham]
    T
    [Circle, Square, Star]
    InTheClutchesOfChaos

hangmansHillShroudedInMystery :: CardDef
hangmansHillShroudedInMystery =
  victory 1
    $ location
      "05304"
      ("Hangman's Hill" <:> "Shrouded In Mystery")
      [Arkham]
      Moon
      [Plus]
      SecretsOfTheUniverse

hangmansHillWhereItAllEnds :: CardDef
hangmansHillWhereItAllEnds =
  location
    "05302"
    ("Hangman's Hill" <:> "Where It All Ends")
    [Arkham]
    Moon
    [Plus]
    MusicOfTheDamned

merchantDistrict_300 :: CardDef
merchantDistrict_300 =
  location
    "05300"
    "Merchant District"
    [Arkham]
    Triangle
    [Circle, Square, Plus]
    InTheClutchesOfChaos

merchantDistrict_301 :: CardDef
merchantDistrict_301 =
  location
    "05301"
    "Merchant District"
    [Arkham]
    Triangle
    [Circle, Square, Plus]
    InTheClutchesOfChaos

rivertown_292 :: CardDef
rivertown_292 =
  location
    "05292"
    "Rivertown"
    [Arkham]
    Circle
    [Square, Triangle, T]
    InTheClutchesOfChaos

rivertown_293 :: CardDef
rivertown_293 =
  location
    "05293"
    "Rivertown"
    [Arkham]
    Circle
    [Square, Triangle, T]
    InTheClutchesOfChaos

silverTwilightLodgeShroudedInMystery :: CardDef
silverTwilightLodgeShroudedInMystery =
  victory 1
    $ location
      "05303"
      ("Silver Twilight Lodge" <:> "Shrouded In Mystery")
      [Arkham]
      Star
      [T]
      MusicOfTheDamned

silverTwilightLodgeWhereItAllEnds :: CardDef
silverTwilightLodgeWhereItAllEnds =
  location
    "05305"
    ("Silver Twilight Lodge" <:> "Where It All Ends")
    [Arkham]
    Star
    [T]
    SecretsOfTheUniverse

southChurch_298 :: CardDef
southChurch_298 =
  location
    "05298"
    "South Church"
    [Arkham]
    Diamond
    [Square]
    InTheClutchesOfChaos

southChurch_299 :: CardDef
southChurch_299 =
  location
    "05299"
    "South Church"
    [Arkham]
    Diamond
    [Square]
    InTheClutchesOfChaos

southside_294 :: CardDef
southside_294 =
  location
    "05294"
    "Southside"
    [Arkham, Central]
    Square
    [Circle, Triangle, Plus, T, Diamond]
    InTheClutchesOfChaos

southside_295 :: CardDef
southside_295 =
  location
    "05295"
    "Southside"
    [Arkham, Central]
    Square
    [Circle, Triangle, Plus, T, Diamond]
    InTheClutchesOfChaos

uptown_296 :: CardDef
uptown_296 =
  location
    "05296"
    "Uptown"
    [Arkham]
    Plus
    [Square, Triangle, Moon]
    InTheClutchesOfChaos

uptown_297 :: CardDef
uptown_297 =
  location
    "05297"
    "Uptown"
    [Arkham]
    Plus
    [Square, Triangle, Moon]
    InTheClutchesOfChaos
