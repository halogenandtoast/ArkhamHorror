module Arkham.Location.CardDefs.TheDreamEaters.WhereTheGodsDwell where

import Arkham.Location.CardDefs.Import

coldWastes :: CardDef
coldWastes =
  location
    "06296"
    "Cold Wastes"
    [Leng]
    Triangle
    [Diamond, Plus, T]
    WhereTheGodsDwell

forsakenTowerOfEternalFlame :: CardDef
forsakenTowerOfEternalFlame =
  locationWithUnrevealed
    "06303"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of Eternal Flame")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

forsakenTowerOfIllusionAndMyth :: CardDef
forsakenTowerOfIllusionAndMyth =
  locationWithUnrevealed
    "06300"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of Illusion and Myth")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

forsakenTowerOfInfiniteTruth :: CardDef
forsakenTowerOfInfiniteTruth =
  locationWithUnrevealed
    "06302"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of Infinite Truth")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

forsakenTowerOfLifeAndDeath :: CardDef
forsakenTowerOfLifeAndDeath =
  locationWithUnrevealed
    "06301"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of Life and Death")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

forsakenTowerOfPrimevalLight :: CardDef
forsakenTowerOfPrimevalLight =
  locationWithUnrevealed
    "06305"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of Primeval Light")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

forsakenTowerOfTheQueenOfNight :: CardDef
forsakenTowerOfTheQueenOfNight =
  locationWithUnrevealed
    "06304"
    "Forsaken Tower"
    [Kadath]
    Equals
    [Square]
    ("Forsaken Tower" <:> "Of the Queen of Night")
    [Kadath]
    Equals
    [Square]
    WhereTheGodsDwell

monasteryOfLeng :: CardDef
monasteryOfLeng =
  location
    "06297"
    "Monastery of Leng"
    [Leng]
    Plus
    [Triangle]
    WhereTheGodsDwell

onyxGates :: CardDef
onyxGates =
  location
    "06298"
    "Onyx Gates"
    [Leng, Kadath]
    T
    [Triangle, Square]
    WhereTheGodsDwell

plateauOfLeng :: CardDef
plateauOfLeng =
  location
    "06295"
    "Plateau of Leng"
    [Leng]
    Diamond
    [Triangle]
    WhereTheGodsDwell

theOnyxCastle :: CardDef
theOnyxCastle =
  locationWithUnrevealed
    "06299"
    "The Onyx Castle"
    [Kadath]
    Square
    [T]
    "The Great Hall"
    [Kadath]
    Square
    [Equals]
    WhereTheGodsDwell
