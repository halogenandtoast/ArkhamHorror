module Arkham.Location.CardDefs.FortuneAndFolly where

import Arkham.Location.CardDefs.Import

baccaratTable :: CardDef
baccaratTable =
  location
    "88012"
    "Baccarat Table"
    [Public, Casino, Game]
    Plus
    [Squiggle, Square, Triangle]
    FortuneAndFolly

casinoFloorBusyNight :: CardDef
casinoFloorBusyNight =
  otherSideIs "88009"
    $ location
      "88009b"
      ("Casino Floor" <:> "Busy Night")
      [Public, Casino]
      Circle
      [Square, Diamond]
      FortuneAndFolly

casinoFloorCalmNight :: CardDef
casinoFloorCalmNight =
  otherSideIs "88009b"
    $ location
      "88009"
      ("Casino Floor" <:> "Calm Night")
      [Public, Casino]
      Circle
      [Square, Diamond]
      FortuneAndFolly

casinoLoungeBusyNight :: CardDef
casinoLoungeBusyNight =
  victory 1
    $ otherSideIs "88015"
    $ location
      "88015b"
      ("Casino Lounge" <:> "Busy Night")
      [Public, Casino]
      T
      [Triangle, Squiggle, Hourglass]
      FortuneAndFolly

casinoLoungeCalmNight :: CardDef
casinoLoungeCalmNight =
  otherSideIs "88015b"
    $ location
      "88015"
      ("Casino Lounge" <:> "Calm Night")
      [Public, Casino]
      T
      [Triangle, Squiggle, Hourglass]
      FortuneAndFolly

countingRoom :: CardDef
countingRoom =
  location
    "88020"
    "Counting Room"
    [Restricted, Casino]
    Star
    [Equals, Heart, Droplet]
    FortuneAndFolly

guardRoom :: CardDef
guardRoom =
  location
    "88018"
    "Guard Room"
    [Restricted, Casino]
    Equals
    [Hourglass, Star, Moon]
    FortuneAndFolly

highRollersTableBusyNight :: CardDef
highRollersTableBusyNight =
  otherSideIs "88014"
    $ location
      "88014b"
      ("High Roller's Table" <:> "Busy Night")
      [Public, Casino, Game]
      Squiggle
      [T, Plus, Hourglass]
      FortuneAndFolly

highRollersTableCalmNight :: CardDef
highRollersTableCalmNight =
  otherSideIs "88014b"
    $ location
      "88014"
      ("High Roller's Table" <:> "Calm Night")
      [Public, Casino, Game]
      Squiggle
      [T, Plus, Hourglass]
      FortuneAndFolly

ownersOffice :: CardDef
ownersOffice =
  victory 1
    $ location
      "88019"
      "Owner's Office"
      [Restricted, Casino]
      Heart
      [Star, Moon, Droplet]
      FortuneAndFolly

pokerTable :: CardDef
pokerTable =
  location
    "88010"
    "Poker Table"
    [Public, Casino, Game]
    Square
    [Plus, Circle]
    FortuneAndFolly

relicRoomSanctumOfFortune :: CardDef
relicRoomSanctumOfFortune =
  location
    "88022"
    ("Relic Room" <:> "Sanctum of Fortune")
    [Restricted, Casino]
    Trefoil
    [Droplet]
    FortuneAndFolly

rouletteWheel :: CardDef
rouletteWheel =
  location
    "88011"
    "Roulette Wheel"
    [Public, Casino, Game]
    Triangle
    [Diamond, T, Plus]
    FortuneAndFolly

securityOffice :: CardDef
securityOffice =
  victory 1
    $ location
      "88017"
      "Security Office"
      [Restricted, Casino]
      Moon
      [Heart, Hourglass, Equals]
      FortuneAndFolly

slotMachines :: CardDef
slotMachines =
  location
    "88013"
    "Slot Machines"
    [Public, Casino, Game]
    Diamond
    [Circle, Triangle]
    FortuneAndFolly

staffAccessHallway :: CardDef
staffAccessHallway =
  location
    "88016"
    "Staff Access Hallway"
    [Restricted, Casino]
    Hourglass
    [Moon, Equals, Squiggle, T]
    FortuneAndFolly

vaultDoor :: CardDef
vaultDoor =
  location
    "88021"
    "Vault Door"
    [Restricted, Casino]
    Droplet
    [Star, Heart, Trefoil]
    FortuneAndFolly
