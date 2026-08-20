module Arkham.Location.CardDefs.TheDreamEaters.DarkSideOfTheMoon where

import Arkham.Location.CardDefs.Import

cavernsBeneathTheMoonDarkSide :: CardDef
cavernsBeneathTheMoonDarkSide =
  location
    "06219"
    ("Caverns Beneath the Moon" <:> "Dark Side")
    [Cave]
    Squiggle
    [Circle, Moon, Star]
    DarkSideOfTheMoon

cavernsBeneathTheMoonLightSide :: CardDef
cavernsBeneathTheMoonLightSide =
  location
    "06221"
    ("Caverns Beneath the Moon" <:> "Light Side")
    [Cave]
    Equals
    [Star, Hourglass]
    DarkSideOfTheMoon

cityOfTheMoonBeasts :: CardDef
cityOfTheMoonBeasts =
  victory 1
    $ location
      "06215"
      "City of the Moon-Beasts"
      [Surface, City]
      Triangle
      [Square, Moon]
      DarkSideOfTheMoon

lightSideOfTheMoon :: CardDef
lightSideOfTheMoon =
  victory 2
    $ location
      "06222"
      "Light Side of the Moon"
      [Surface, Ruins]
      Hourglass
      [Equals, Heart]
      DarkSideOfTheMoon

moonBeastGalley :: CardDef
moonBeastGalley =
  storyOnBack
    $ location
      "06214"
      "Moon-Beast Galley"
      [Ship]
      NoSymbol
      []
      DarkSideOfTheMoon

moonForest :: CardDef
moonForest =
  victory 1
    $ location
      "06218"
      "Moon-Forest"
      [Surface, Woods]
      Circle
      [Moon, Square, Squiggle]
      DarkSideOfTheMoon

templeOfTheMoonLizard :: CardDef
templeOfTheMoonLizard =
  victory 1
    $ location
      "06217"
      "Temple of the Moon Lizard"
      [Surface]
      Square
      [Circle, Triangle]
      DarkSideOfTheMoon

theBlackCore :: CardDef
theBlackCore =
  location
    "06220"
    "The Black Core"
    [Cave]
    Star
    [Squiggle, Equals]
    DarkSideOfTheMoon

theDarkCrater :: CardDef
theDarkCrater =
  victory 1
    $ location
      "06216"
      "The Dark Crater"
      [Surface]
      Moon
      [Triangle, Circle, Squiggle]
      DarkSideOfTheMoon

theWhiteShip :: CardDef
theWhiteShip =
  location
    "06223"
    "The White Ship"
    [Ship]
    Heart
    [Hourglass]
    DarkSideOfTheMoon
