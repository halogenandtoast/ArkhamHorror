module Arkham.Location.CardDefs.TheScarletKeys.DeadHeat where

import Arkham.Location.CardDefs.Import

bahiaPalaceGardens :: CardDef
bahiaPalaceGardens =
  locationWithUnrevealed
    "09530"
    "Bahia Palace Gardens"
    [Marrakesh]
    Square
    [Squiggle, Triangle]
    "Bahia Palace Gardens"
    [Marrakesh, RitualSite]
    Square
    [Squiggle, Triangle]
    DeadHeat

bahiaPalaceGardensAbandoned :: CardDef
bahiaPalaceGardensAbandoned =
  locationWithUnrevealed
    "09535"
    "Bahia Palace Gardens"
    [Marrakesh, Abandoned]
    Square
    [Squiggle, Triangle]
    "Bahia Palace Gardens"
    [Marrakesh, RitualSite, Abandoned]
    Square
    [Squiggle, Triangle]
    DeadHeat

jemaaElFnaaSquare :: CardDef
jemaaElFnaaSquare =
  location
    "09527"
    "Jemaa el-Fnaa Square"
    [Marrakesh]
    Diamond
    [Squiggle, Heart, Triangle]
    DeadHeat

jemaaElFnaaSquareAbandoned :: CardDef
jemaaElFnaaSquareAbandoned =
  location
    "09532"
    "Jemaa el-Fnaa Square"
    [Marrakesh, Abandoned]
    Diamond
    [Squiggle, Heart, Triangle]
    DeadHeat

marrakeshRailwayStation :: CardDef
marrakeshRailwayStation =
  location
    "09526"
    "Marrakesh Railway Station"
    [Marrakesh]
    Squiggle
    [Diamond, Heart, Square]
    DeadHeat

marrakeshRailwayStationAbandoned :: CardDef
marrakeshRailwayStationAbandoned =
  location
    "09531"
    "Marrakesh Railway Station"
    [Marrakesh, Abandoned]
    Squiggle
    [Diamond, Heart, Square]
    DeadHeat

saadiansTombs :: CardDef
saadiansTombs =
  location
    "09528"
    "Saadian's Tombs"
    [Marrakesh]
    Heart
    [Squiggle, Diamond]
    DeadHeat

saadiansTombsAbandoned :: CardDef
saadiansTombsAbandoned =
  location
    "09533"
    "Saadian's Tombs"
    [Marrakesh, Abandoned]
    Heart
    [Squiggle, Diamond]
    DeadHeat

tanneries :: CardDef
tanneries =
  location
    "09529"
    "Tanneries"
    [Marrakesh]
    Triangle
    [Diamond, Square]
    DeadHeat

tanneriesAbandoned :: CardDef
tanneriesAbandoned =
  location
    "09534"
    "Tanneries"
    [Marrakesh, Abandoned]
    Triangle
    [Diamond, Square]
    DeadHeat
