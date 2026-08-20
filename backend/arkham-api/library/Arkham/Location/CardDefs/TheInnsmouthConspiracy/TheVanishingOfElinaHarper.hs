module Arkham.Location.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper where

import Arkham.Location.CardDefs.Import

esotericOrderOfDagon :: CardDef
esotericOrderOfDagon =
  singleSided
    $ victory 1
    $ location
      "07070"
      "Esoteric Order of Dagon"
      [Innsmouth, Hideout]
      Plus
      [Droplet, Star]
      TheVanishingOfElinaHarper

firstNationalGrocery :: CardDef
firstNationalGrocery =
  location
    "07067"
    "First National Grocery"
    [Innsmouth]
    Star
    [Triangle, Diamond, Droplet, Square, Plus]
    TheVanishingOfElinaHarper

fishStreetBridge :: CardDef
fishStreetBridge =
  location
    "07068"
    "Fish Street Bridge"
    [Innsmouth]
    Hourglass
    [Triangle, Circle, Moon, Equals, T]
    TheVanishingOfElinaHarper

gilmanHouse :: CardDef
gilmanHouse =
  location
    "07064"
    "Gilman House"
    [Innsmouth]
    Diamond
    [Triangle, Droplet, Star, Squiggle, T]
    TheVanishingOfElinaHarper

innsmouthHarbour :: CardDef
innsmouthHarbour =
  location
    "07066"
    "Innsmouth Harbour"
    [Innsmouth]
    Moon
    [Circle, Hourglass, Equals, Heart]
    TheVanishingOfElinaHarper

innsmouthJail :: CardDef
innsmouthJail =
  singleSided
    $ victory 1
    $ location
      "07074"
      "Innsmouth Jail"
      [Innsmouth, Hideout]
      T
      [Diamond, Triangle, Hourglass]
      TheVanishingOfElinaHarper

innsmouthSquare :: CardDef
innsmouthSquare =
  location
    "07065"
    "Innsmouth Square"
    [Innsmouth, Central]
    Triangle
    [Circle, Diamond, Star, Hourglass, Square, T]
    TheVanishingOfElinaHarper

marshRefinery :: CardDef
marshRefinery =
  location
    "07063"
    "Marsh Refinery"
    [Innsmouth]
    Circle
    [Triangle, Hourglass, Moon, Square, Heart]
    TheVanishingOfElinaHarper

newChurchGreen :: CardDef
newChurchGreen =
  singleSided
    $ victory 1
    $ location
      "07075"
      "New Church Green"
      [Innsmouth, Hideout]
      Square
      [Circle, Triangle, Star]
      TheVanishingOfElinaHarper

sawboneAlley :: CardDef
sawboneAlley =
  singleSided
    $ victory 1
    $ location
      "07071"
      "Sawbone Alley"
      [Innsmouth, Hideout]
      Squiggle
      [Droplet, Diamond]
      TheVanishingOfElinaHarper

shorewardSlums :: CardDef
shorewardSlums =
  singleSided
    $ victory 1
    $ location
      "07072"
      "Shoreward Slums"
      [Innsmouth, Hideout]
      Equals
      [Hourglass, Moon]
      TheVanishingOfElinaHarper

theHouseOnWaterStreet :: CardDef
theHouseOnWaterStreet =
  singleSided
    $ victory 1
    $ location
      "07073"
      "The House on Water Street"
      [Innsmouth, Hideout]
      Heart
      [Circle, Moon]
      TheVanishingOfElinaHarper

theLittleBookshop :: CardDef
theLittleBookshop =
  location
    "07069"
    "The Little Bookshop"
    [Innsmouth]
    Droplet
    [Diamond, Star, Squiggle, Plus]
    TheVanishingOfElinaHarper
