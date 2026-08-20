{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheDunwichLegacy.TheMiskatonicMuseum where

import Arkham.Location.CardDefs.Import

administrationOffice_130 :: CardDef
administrationOffice_130 =
  location
    "02130"
    "Administration Office"
    [Miskatonic]
    Triangle
    [Square]
    TheMiskatonicMuseum

administrationOffice_131 :: CardDef
administrationOffice_131 =
  location
    "02131"
    "Administration Office"
    [Miskatonic]
    Triangle
    [Square]
    TheMiskatonicMuseum

exhibitHallAthabaskanExhibit :: CardDef
exhibitHallAthabaskanExhibit =
  locationWithUnrevealed
    "02132"
    "Exhibit Hall"
    [Miskatonic, Exhibit]
    NoSymbol
    [Square]
    ("Exhibit Hall" <:> "Athabaskan Exhibit")
    [Miskatonic, Exhibit]
    Plus
    [Square]
    TheMiskatonicMuseum

exhibitHallEgyptianExhibit :: CardDef
exhibitHallEgyptianExhibit =
  victory 1
    $ locationWithUnrevealed
      "02135"
      "Exhibit Hall"
      [Miskatonic, Exhibit]
      NoSymbol
      [Square]
      ("Exhibit Hall" <:> "Egyptian Exhibit")
      [Miskatonic, Exhibit]
      Moon
      [Square, T]
      TheMiskatonicMuseum

exhibitHallHallOfTheDead :: CardDef
exhibitHallHallOfTheDead =
  victory 1
    $ locationWithUnrevealed
      "02136"
      "Exhibit Hall"
      [Miskatonic, Exhibit]
      NoSymbol
      [Square]
      ("Exhibit Hall" <:> "Hall of the Dead")
      [Miskatonic, Exhibit]
      Squiggle
      [Square, Hourglass]
      TheMiskatonicMuseum

exhibitHallMedusaExhibit :: CardDef
exhibitHallMedusaExhibit =
  victory 1
    $ locationWithUnrevealed
      "02133"
      "Exhibit Hall"
      [Miskatonic, Exhibit]
      NoSymbol
      [Square]
      ("Exhibit Hall" <:> "Medusa Exhibit")
      [Miskatonic, Exhibit]
      T
      [Square, Moon]
      TheMiskatonicMuseum

exhibitHallNatureExhibit :: CardDef
exhibitHallNatureExhibit =
  victory 1
    $ locationWithUnrevealed
      "02134"
      "Exhibit Hall"
      [Miskatonic, Exhibit]
      NoSymbol
      [Square]
      ("Exhibit Hall" <:> "Nature Exhibit")
      [Miskatonic, Exhibit]
      Hourglass
      [Square, Squiggle]
      TheMiskatonicMuseum

exhibitHallRestrictedHall :: CardDef
exhibitHallRestrictedHall =
  victory 1
    $ locationWithUnrevealed
      "02137"
      "Exhibit Hall"
      [Miskatonic, Exhibit]
      NoSymbol
      [Square]
      ("Exhibit Hall" <:> "Restricted Hall")
      [Miskatonic, Exhibit]
      Equals
      [Square]
      TheMiskatonicMuseum

museumEntrance :: CardDef
museumEntrance =
  location
    "02126"
    "Museum Entrance"
    [Miskatonic]
    Circle
    [Square]
    TheMiskatonicMuseum

museumHalls :: CardDef
museumHalls =
  location
    "02127"
    "Museum Halls"
    [Miskatonic]
    Square
    [Circle, Diamond, Triangle]
    TheMiskatonicMuseum

securityOffice_128 :: CardDef
securityOffice_128 =
  location
    "02128"
    "Security Office"
    [Miskatonic]
    Diamond
    [Square]
    TheMiskatonicMuseum

securityOffice_129 :: CardDef
securityOffice_129 =
  location
    "02129"
    "Security Office"
    [Miskatonic]
    Diamond
    [Square]
    TheMiskatonicMuseum
