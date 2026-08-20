module Arkham.Location.CardDefs.TheInnsmouthConspiracy.ALightInTheFog where

import Arkham.Location.CardDefs.Import

deepOneNursery :: CardDef
deepOneNursery =
  locationWithUnrevealed
    "07248"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Deep One Nursery"
    [Cave]
    NoSymbol
    []
    ALightInTheFog

falconPointCliffside :: CardDef
falconPointCliffside =
  location
    "07242"
    "Falcon Point Cliffside"
    [FalconPoint]
    NoSymbol
    []
    ALightInTheFog

falconPointGatehouse :: CardDef
falconPointGatehouse =
  location
    "07239"
    "Falcon Point Gatehouse"
    [FalconPoint]
    NoSymbol
    []
    ALightInTheFog

holdingCells :: CardDef
holdingCells =
  victory 1
    $ otherSideIs "07252"
    $ location
      "07252b"
      "Holding Cells"
      [Cave]
      T
      [Squiggle, Square, Diamond]
      ALightInTheFog

lanternRoom :: CardDef
lanternRoom =
  victory 1
    $ location
      "07241"
      "Lantern Room"
      [FalconPoint]
      Triangle
      [Equals]
      ALightInTheFog

lighthouseKeepersCottage :: CardDef
lighthouseKeepersCottage =
  location
    "07243"
    "Lighthouse Keeper's Cottage"
    [FalconPoint]
    NoSymbol
    []
    ALightInTheFog

lighthouseStairwell :: CardDef
lighthouseStairwell =
  location
    "07240"
    "Lighthouse Stairwell"
    [FalconPoint]
    Equals
    [Triangle, Squiggle]
    ALightInTheFog

pumpRoom :: CardDef
pumpRoom =
  locationWithUnrevealed
    "07251"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Pump Room"
    [Cave]
    NoSymbol
    []
    ALightInTheFog

shrineToHydra :: CardDef
shrineToHydra =
  victory 1
    $ locationWithUnrevealed
      "07247"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Shrine to Hydra"
      [Cave]
      NoSymbol
      []
      ALightInTheFog

sunkenArchives :: CardDef
sunkenArchives =
  victory 1
    $ locationWithUnrevealed_
      "07250"
      "Tidal Tunnel"
      [Cave]
      "Sunken Archives"
      [Cave]
      ALightInTheFog

sunkenGrottoFinalDepths :: CardDef
sunkenGrottoFinalDepths =
  location
    "07246"
    ("Sunken Grotto" <:> "Final Depths")
    [Cave]
    Diamond
    [Square, T]
    ALightInTheFog

sunkenGrottoLowerDepths :: CardDef
sunkenGrottoLowerDepths =
  location
    "07245"
    ("Sunken Grotto" <:> "Lower Depths")
    [Cave]
    Square
    [Squiggle, Diamond, T]
    ALightInTheFog

sunkenGrottoUpperDepths :: CardDef
sunkenGrottoUpperDepths =
  locationWithUnrevealed
    "07244"
    "Lighthouse Basement"
    [Cave]
    Squiggle
    [Equals, Square, T]
    ("Sunken Grotto" <:> "Upper Depths")
    [Cave]
    Squiggle
    [Equals, Square, T]
    ALightInTheFog

theMoonRoom :: CardDef
theMoonRoom =
  locationWithUnrevealed
    "07249"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "The Moon Room"
    [Cave]
    NoSymbol
    []
    ALightInTheFog
