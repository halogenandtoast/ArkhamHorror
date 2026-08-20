module Arkham.Location.CardDefs.TheDreamEaters.BeyondTheGatesOfSleep where

import Arkham.Location.CardDefs.Import

baseOfTheSteps :: CardDef
baseOfTheSteps =
  location
    "06048"
    "Base of the Steps"
    [Steps, Woods]
    T
    [Equals, Squiggle]
    BeyondTheGatesOfSleep

enchantedWoodsFungalForest :: CardDef
enchantedWoodsFungalForest =
  victory 2
    $ locationWithUnrevealed
      "06055"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Fungal Forest")
      [Woods]
      Plus
      [Squiggle, Circle, Triangle]
      BeyondTheGatesOfSleep

enchantedWoodsGreatStoneCircle :: CardDef
enchantedWoodsGreatStoneCircle =
  victory 1
    $ locationWithUnrevealed
      "06052"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Great Stone Circle")
      [Woods]
      Triangle
      [Squiggle, Plus, Square]
      BeyondTheGatesOfSleep

enchantedWoodsLostWoods :: CardDef
enchantedWoodsLostWoods =
  victory 2
    $ locationWithUnrevealed
      "06056"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Lost Woods")
      [Woods]
      Droplet
      [Squiggle]
      BeyondTheGatesOfSleep

enchantedWoodsMysticalForest :: CardDef
enchantedWoodsMysticalForest =
  victory 2
    $ locationWithUnrevealed
      "06050"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Mystical Forest")
      [Woods]
      Circle
      [Squiggle, Star, Plus]
      BeyondTheGatesOfSleep

enchantedWoodsStoneTrapdoor :: CardDef
enchantedWoodsStoneTrapdoor =
  victory 2
    $ locationWithUnrevealed
      "06053"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Stone Trapdoor")
      [Woods]
      Square
      [Squiggle, Triangle, Diamond]
      BeyondTheGatesOfSleep

enchantedWoodsTheMoonTree :: CardDef
enchantedWoodsTheMoonTree =
  victory 2
    $ locationWithUnrevealed
      "06054"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "The Moon-Tree")
      [Woods]
      Star
      [Squiggle, Diamond, Circle]
      BeyondTheGatesOfSleep

enchantedWoodsVillageOfZoogs :: CardDef
enchantedWoodsVillageOfZoogs =
  victory 2
    $ locationWithUnrevealed
      "06051"
      "Enchanted Woods"
      [Woods]
      Moon
      [Squiggle]
      ("Enchanted Woods" <:> "Village of Zoogs")
      [Woods]
      Diamond
      [Squiggle, Square, Star]
      BeyondTheGatesOfSleep

sevenHundredSteps :: CardDef
sevenHundredSteps =
  location
    "06047"
    ("Seven Hundred Steps" <:> "Of Deeper Slumber")
    [Steps]
    Equals
    [Hourglass, T]
    BeyondTheGatesOfSleep

seventySteps :: CardDef
seventySteps =
  location
    "06045"
    ("Seventy Steps" <:> "Of Lighter Slumber")
    [Steps]
    Heart
    [Hourglass]
    BeyondTheGatesOfSleep

theCavernOfFlame :: CardDef
theCavernOfFlame =
  location
    "06046"
    "The Cavern of Flame"
    [Cave, Steps]
    Hourglass
    [Heart, Equals]
    BeyondTheGatesOfSleep

theEnchantedPath :: CardDef
theEnchantedPath =
  location
    "06049"
    "The Enchanted Path"
    [Woods]
    Squiggle
    [T, Moon]
    BeyondTheGatesOfSleep
