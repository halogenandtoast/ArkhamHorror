{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.ThePathToCarcosa.BlackStarsRise where

import Arkham.Location.CardDefs.Import

abbeyChurch :: CardDef
abbeyChurch =
  victory 1
    $ location
      "03291"
      "Abbey Church"
      []
      Square
      [Equals, T, Heart, Hourglass, Moon]
      BlackStarsRise

abbeyTowerSpiresForbidden :: CardDef
abbeyTowerSpiresForbidden =
  victory 2
    $ locationWithUnrevealed
      "03299"
      "Abbey Tower"
      []
      Star
      [T]
      ("Abbey Tower" <:> "Spires Forbidden")
      []
      Star
      [T]
      BlackStarsRise

abbeyTowerThePathIsOpen :: CardDef
abbeyTowerThePathIsOpen =
  locationWithUnrevealed
    "03298"
    "Abbey Tower"
    []
    Star
    [T]
    ("Abbey Tower" <:> "The Path is Open")
    []
    Star
    [T]
    BlackStarsRise

brokenSteps_289 :: CardDef
brokenSteps_289 =
  location
    "03289"
    "Broken Steps"
    []
    Equals
    [Squiggle, Triangle, Diamond, Square]
    BlackStarsRise

brokenSteps_290 :: CardDef
brokenSteps_290 =
  location
    "03290"
    "Broken Steps"
    []
    Equals
    [Squiggle, Triangle, Diamond, Square]
    BlackStarsRise

chapelOfStAubertThePathIsOpen :: CardDef
chapelOfStAubertThePathIsOpen =
  locationWithUnrevealed
    "03296"
    "Chapel of St. Aubert"
    []
    Moon
    [Square]
    ("Chapel of St. Aubert" <:> "The Path is Open")
    []
    Moon
    [Square]
    BlackStarsRise

chapelOfStAubertWatersForbidden :: CardDef
chapelOfStAubertWatersForbidden =
  victory 2
    $ locationWithUnrevealed
      "03297"
      "Chapel of St. Aubert"
      []
      Moon
      [Square]
      ("Chapel of St. Aubert" <:> "Waters Forbidden")
      []
      Moon
      [Square]
      BlackStarsRise

choeurGothique_292 :: CardDef
choeurGothique_292 =
  location "03292" "Chœur Gothique" [] T [Square, Star] BlackStarsRise

choeurGothique_293 :: CardDef
choeurGothique_293 =
  location "03293" "Chœur Gothique" [] T [Square, Star] BlackStarsRise

cloister :: CardDef
cloister =
  location "03294" "Cloister" [] Heart [Square, Hourglass] BlackStarsRise

grandRue :: CardDef
grandRue =
  location
    "03284"
    "Grand Rue"
    []
    Squiggle
    [Circle, Triangle, Diamond, Equals]
    BlackStarsRise

knightsHall :: CardDef
knightsHall =
  location "03295" "Knight's Hall" [] Hourglass [Square, Heart] BlackStarsRise

northTower_287 :: CardDef
northTower_287 =
  victory 1
    $ location
      "03287"
      "North Tower"
      []
      Diamond
      [Squiggle, Triangle, Equals]
      BlackStarsRise

northTower_288 :: CardDef
northTower_288 =
  victory 1
    $ location
      "03288"
      "North Tower"
      []
      Diamond
      [Squiggle, Triangle, Equals]
      BlackStarsRise

outerWall_285 :: CardDef
outerWall_285 =
  victory 1
    $ location
      "03285"
      "Outer Wall"
      []
      Triangle
      [Squiggle, Diamond, Equals]
      BlackStarsRise

outerWall_286 :: CardDef
outerWall_286 =
  victory 1
    $ location
      "03286"
      "Outer Wall"
      []
      Triangle
      [Squiggle, Diamond, Equals]
      BlackStarsRise

porteDeLAvancee :: CardDef
porteDeLAvancee =
  location "03283" "Porte de l'Avancée" [] Circle [Squiggle] BlackStarsRise
