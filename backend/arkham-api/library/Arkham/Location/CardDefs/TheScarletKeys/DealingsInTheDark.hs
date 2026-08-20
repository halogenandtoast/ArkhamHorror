module Arkham.Location.CardDefs.TheScarletKeys.DealingsInTheDark where

import Arkham.Location.CardDefs.Import

galata :: CardDef
galata =
  location
    "09573"
    "Galata"
    [Istanbul]
    Plus
    [Moon, Circle, Squiggle]
    DealingsInTheDark

galataDocks :: CardDef
galataDocks =
  victory 1
    $ location
      "09572"
      "Galata Docks"
      [Istanbul]
      Squiggle
      [Diamond, Plus]
      DealingsInTheDark

grandBazaarBusyWalkway :: CardDef
grandBazaarBusyWalkway =
  locationWithUnrevealed_
    "09581"
    "Grand Bazaar"
    [Istanbul, Bazaar]
    ("Grand Bazaar" <:> "Busy Walkway")
    [Istanbul, Bazaar]
    DealingsInTheDark

grandBazaarCrowdedShops :: CardDef
grandBazaarCrowdedShops =
  locationWithUnrevealed_
    "09580"
    "Grand Bazaar"
    [Istanbul, Bazaar]
    ("Grand Bazaar" <:> "Crowded Shops")
    [Istanbul, Bazaar]
    DealingsInTheDark

grandBazaarDarkenedAlley :: CardDef
grandBazaarDarkenedAlley =
  locationWithUnrevealed_
    "09577"
    "Grand Bazaar"
    [Istanbul, Bazaar]
    ("Grand Bazaar" <:> "Darkened Alley")
    [Istanbul, Bazaar]
    DealingsInTheDark

grandBazaarJewelersRoad :: CardDef
grandBazaarJewelersRoad =
  locationWithUnrevealed_
    "09583"
    "Grand Bazaar"
    [Istanbul, Bazaar]
    ("Grand Bazaar" <:> "Jewelers' Road")
    [Istanbul, Bazaar]
    DealingsInTheDark

grandBazaarMarbleFountain :: CardDef
grandBazaarMarbleFountain =
  victory 1
    $ locationWithUnrevealed_
      "09579"
      "Grand Bazaar"
      [Istanbul, Bazaar]
      ("Grand Bazaar" <:> "Marble Fountain")
      [Istanbul, Bazaar]
      DealingsInTheDark

grandBazaarPublicBaths :: CardDef
grandBazaarPublicBaths =
  locationWithUnrevealed_
    "09578"
    "Grand Bazaar"
    [Istanbul, Bazaar]
    ("Grand Bazaar" <:> "Public Baths")
    [Istanbul, Bazaar]
    DealingsInTheDark

grandBazaarRooftopAccess :: CardDef
grandBazaarRooftopAccess =
  victory 1
    $ locationWithUnrevealed_
      "09582"
      "Grand Bazaar"
      [Istanbul, Bazaar]
      ("Grand Bazaar" <:> "Rooftop Access")
      [Istanbul, Bazaar]
      DealingsInTheDark

hagiaSophia :: CardDef
hagiaSophia =
  location
    "09576"
    "Hagia Sophia"
    [Istanbul]
    Square
    [Moon, Circle]
    DealingsInTheDark

istanbulUniversity :: CardDef
istanbulUniversity =
  victory 1
    $ location
      "09575"
      "Istanbul University"
      [Istanbul]
      Circle
      [Plus, Square]
      DealingsInTheDark

obeliskOfTheodosius :: CardDef
obeliskOfTheodosius =
  victory 1
    $ location
      "09574"
      "Obelisk of Theodosius"
      [Istanbul]
      Moon
      [Plus, Square]
      DealingsInTheDark
