module Arkham.Location.CardDefs.TheCircleUndone.BeforeTheBlackThrone where

import Arkham.Location.CardDefs.Import

cosmicGate :: CardDef
cosmicGate =
  locationWithUnrevealed
    "05339"
    "Cosmos"
    [Otherworld]
    NoSymbol
    []
    "Cosmic Gate"
    [Otherworld, Void]
    NoSymbol
    []
    BeforeTheBlackThrone

cosmicIngress :: CardDef
cosmicIngress =
  location
    "05332"
    "Cosmic Ingress"
    [Otherworld]
    NoSymbol
    []
    BeforeTheBlackThrone

courtOfTheGreatOldOnes :: CardDef
courtOfTheGreatOldOnes =
  locationWithUnrevealed
    "05334"
    "Cosmos"
    [Otherworld]
    NoSymbol
    []
    "Court of the Great Old Ones"
    [Otherworld]
    NoSymbol
    []
    BeforeTheBlackThrone

dancersMist :: CardDef
dancersMist =
  quantity 3
    $ locationWithUnrevealed
      "05336"
      "Cosmos"
      [Otherworld]
      NoSymbol
      []
      "Dancer's Mist"
      [Otherworld, Void]
      NoSymbol
      []
      BeforeTheBlackThrone

flightIntoOblivion :: CardDef
flightIntoOblivion =
  quantity 3
    $ locationWithUnrevealed
      "05337"
      "Cosmos"
      [Otherworld]
      NoSymbol
      []
      "Flight into Oblivion"
      [Otherworld, Void]
      NoSymbol
      []
      BeforeTheBlackThrone

hideousPalace :: CardDef
hideousPalace =
  locationWithUnrevealed
    "05333"
    "Cosmos"
    [Otherworld]
    NoSymbol
    []
    "Hideous Palace"
    [Otherworld]
    NoSymbol
    []
    BeforeTheBlackThrone

infinityOfDarkness :: CardDef
infinityOfDarkness =
  quantity 3
    $ locationWithUnrevealed
      "05338"
      "Cosmos"
      [Otherworld]
      NoSymbol
      []
      "Infinity of Darkness"
      [Otherworld, Void]
      NoSymbol
      []
      BeforeTheBlackThrone

pathwayIntoVoid :: CardDef
pathwayIntoVoid =
  quantity 2
    $ locationWithUnrevealed
      "05340"
      "Cosmos"
      [Otherworld]
      NoSymbol
      []
      "Pathway into Void"
      [Otherworld, Void]
      NoSymbol
      []
      BeforeTheBlackThrone

theBlackThrone :: CardDef
theBlackThrone =
  locationWithUnrevealed
    "05335"
    "Cosmos"
    [Otherworld]
    NoSymbol
    []
    "The Black Throne"
    [Otherworld]
    NoSymbol
    []
    BeforeTheBlackThrone
