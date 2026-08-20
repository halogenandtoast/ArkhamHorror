module Arkham.Location.CardDefs.TheForgottenAge.TheCityOfArchives where

import Arkham.Location.CardDefs.Import

deconstructionRoom :: CardDef
deconstructionRoom =
  victory 1
    $ location
      "04254"
      "Deconstruction Room"
      [Ancient, Pnakotus]
      Equals
      [Triangle]
      TheCityOfArchives

greatLibrary :: CardDef
greatLibrary =
  location
    "04251"
    "Great Library"
    [Ancient, Pnakotus]
    Circle
    [Square]
    TheCityOfArchives

hallsOfPnakotusEasternCorridors :: CardDef
hallsOfPnakotusEasternCorridors =
  location
    "04249"
    ("Halls of Pnakotus" <:> "Eastern Corridors")
    [Ancient, Pnakotus]
    Diamond
    [Squiggle, Square, Droplet]
    TheCityOfArchives

hallsOfPnakotusNorthernCorridors :: CardDef
hallsOfPnakotusNorthernCorridors =
  location
    "04248"
    ("Halls of Pnakotus" <:> "Northern Corridors")
    [Ancient, Pnakotus]
    Squiggle
    [Diamond, Square, Triangle]
    TheCityOfArchives

hallsOfPnakotusWesternCorridors :: CardDef
hallsOfPnakotusWesternCorridors =
  location
    "04250"
    ("Halls of Pnakotus" <:> "Western Corridors")
    [Ancient, Pnakotus]
    Square
    [Squiggle, Diamond, Circle, Star]
    TheCityOfArchives

interviewRoomArrivalChamber :: CardDef
interviewRoomArrivalChamber =
  locationWithUnrevealed
    "04245"
    "Interview Room"
    [Ancient, Pnakotus]
    Droplet
    [Diamond]
    ("Interview Room" <:> "Arrival Chamber")
    [Ancient, Pnakotus]
    Droplet
    [Diamond]
    TheCityOfArchives

interviewRoomIchorFilledChamber :: CardDef
interviewRoomIchorFilledChamber =
  victory 1
    $ locationWithUnrevealed
      "04247"
      "Interview Room"
      [Ancient, Pnakotus]
      Droplet
      [Diamond]
      ("Interview Room" <:> "Ichor-Filled Chamber")
      [Ancient, Pnakotus]
      Droplet
      [Diamond]
      TheCityOfArchives

interviewRoomRestrainingChamber :: CardDef
interviewRoomRestrainingChamber =
  victory 1
    $ locationWithUnrevealed
      "04246"
      "Interview Room"
      [Ancient, Pnakotus]
      Droplet
      [Diamond]
      ("Interview Room" <:> "Restraining Chamber")
      [Ancient, Pnakotus]
      Droplet
      [Diamond]
      TheCityOfArchives

laboratoryOfTheGreatRace :: CardDef
laboratoryOfTheGreatRace =
  location
    "04253"
    "Laboratory of the Great Race"
    [Ancient, Pnakotus]
    Triangle
    [Squiggle, Moon, Equals]
    TheCityOfArchives

towersOfPnakotus :: CardDef
towersOfPnakotus =
  victory 2
    $ location
      "04255"
      "Towers of Pnakotus"
      [Ancient, Pnakotus]
      Star
      [Square]
      TheCityOfArchives

yithianOrrery :: CardDef
yithianOrrery =
  victory 1
    $ location
      "04252"
      "Yithian Orrery"
      [Ancient, Pnakotus]
      Moon
      [Triangle]
      TheCityOfArchives
