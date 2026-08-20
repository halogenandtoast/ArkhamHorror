module Arkham.Location.CardDefs.TheCircleUndone.TheSecretName where

import Arkham.Location.CardDefs.Import

cityOfElderThings :: CardDef
cityOfElderThings =
  victory 1
    $ locationWithUnrevealed
      "05136"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      "City of Elder Things"
      [Extradimensional, Otherworld]
      Moon
      [Square]
      TheSecretName

courtOfTheGreatOldOnesANotTooDistantFuture :: CardDef
courtOfTheGreatOldOnesANotTooDistantFuture =
  victory 1
    $ locationWithUnrevealed
      "05140"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      ("Court of the Great Old Ones" <:> "A Not-Too-Distant Future")
      [Extradimensional, Otherworld]
      Squiggle
      [Square, Equals]
      TheSecretName

frankElwoodsRoom :: CardDef
frankElwoodsRoom =
  locationWithUnrevealed
    "05131"
    "Decrepit Door"
    [WitchHouse]
    Plus
    [Triangle]
    "Frank Elwood's room"
    [WitchHouse]
    Diamond
    [Triangle]
    TheSecretName

joeMazurewiczsRoom :: CardDef
joeMazurewiczsRoom =
  locationWithUnrevealed
    "05130"
    "Decrepit Door"
    [WitchHouse]
    Plus
    [Triangle]
    "Joe Mazurewicz's Room"
    [WitchHouse]
    Heart
    [Triangle]
    TheSecretName

keziahsRoom :: CardDef
keziahsRoom =
  location
    "05133"
    "Keziah's Room"
    [Spectral, WitchHouse]
    Square
    [Moon, Hourglass, T, Equals, Squiggle]
    TheSecretName

landlordsQuarters :: CardDef
landlordsQuarters =
  locationWithUnrevealed
    "05129"
    "Decrepit Door"
    [WitchHouse]
    Plus
    [Triangle]
    "Landlord's Quarters"
    [WitchHouse]
    Circle
    [Triangle]
    TheSecretName

moldyHalls :: CardDef
moldyHalls =
  location
    "05128"
    "Moldy Halls"
    [WitchHouse]
    Triangle
    [Plus, Circle, Heart, Square, Diamond]
    TheSecretName

moldyHallsEarlierTonight :: CardDef
moldyHallsEarlierTonight =
  locationWithUnrevealed
    "05134"
    "Unknown Places"
    [Extradimensional]
    Moon
    [Square]
    ("Moldy Halls" <:> "Earlier Tonight")
    [Extradimensional, WitchHouse]
    Moon
    [Square]
    TheSecretName

physicsClassroom :: CardDef
physicsClassroom =
  victory 1
    $ locationWithUnrevealed
      "05139"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      "Physics Classroom"
      [Extradimensional, Miskatonic]
      Moon
      [Square]
      TheSecretName

salemGaol1692 :: CardDef
salemGaol1692 =
  locationWithUnrevealed
    "05138"
    "Unknown Places"
    [Extradimensional]
    Moon
    [Square]
    "Salem Gaol, 1692"
    [Extradimensional, Salem]
    Moon
    [Square]
    TheSecretName

siteOfTheSacrifice :: CardDef
siteOfTheSacrifice =
  location
    "05141"
    "Site of the Sacrifice"
    [Extradimensional, WitchHouse]
    T
    [Hourglass]
    TheSecretName

strangeGeometry :: CardDef
strangeGeometry =
  quantity 2
    $ singleSided
    $ location
      "05142"
      "Strange Geometry"
      [Extradimensional]
      NoSymbol
      []
      TheSecretName

twilightAbyss :: CardDef
twilightAbyss =
  victory 1
    $ locationWithUnrevealed
      "05135"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      "Twilight Abyss"
      [Extradimensional, Otherworld]
      Equals
      [Square, Squiggle]
      TheSecretName

walterGilmansRoom :: CardDef
walterGilmansRoom =
  location
    "05132"
    "Walter Gilman's Room"
    [WitchHouse]
    Square
    [Triangle]
    TheSecretName

witchHouseRuins :: CardDef
witchHouseRuins =
  locationWithUnrevealed
    "05137"
    "Unknown Places"
    [Extradimensional]
    Moon
    [Square]
    "Witch House Ruins"
    [Extradimensional, WitchHouse]
    Hourglass
    [Square, T]
    TheSecretName
