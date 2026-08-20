module Arkham.Location.CardDefs.ReturnToTheCircleUndone.ReturnToTheSecretName where

import Arkham.Location.CardDefs.Import

libraryOfEbla :: CardDef
libraryOfEbla =
  victory 1
    $ locationWithUnrevealed
      "54033"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      "Library of Ebla"
      [Extradimensional]
      Squiggle
      [Square, Equals]
      ReturnToTheSecretName

templeOfRlyeh :: CardDef
templeOfRlyeh =
  victory 1
    $ locationWithUnrevealed
      "54030"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      "Temple of R'lyeh"
      [Extradimensional, Otherworld]
      Equals
      [Square, Squiggle]
      ReturnToTheSecretName

the9thWard :: CardDef
the9thWard =
  locationWithUnrevealed
    "54032"
    "Unknown Places"
    [Extradimensional]
    Moon
    [Square]
    "The 9th Ward"
    [Extradimensional]
    Moon
    [Square]
    ReturnToTheSecretName

thePriceManor :: CardDef
thePriceManor =
  locationWithUnrevealed
    "54031"
    "Unknown Places"
    [Extradimensional]
    Moon
    [Square]
    "The Price Manor"
    [Extradimensional]
    Moon
    [Square]
    ReturnToTheSecretName
