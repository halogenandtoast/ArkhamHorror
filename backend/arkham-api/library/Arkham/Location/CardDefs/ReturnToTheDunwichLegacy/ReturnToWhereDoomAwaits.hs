module Arkham.Location.CardDefs.ReturnToTheDunwichLegacy.ReturnToWhereDoomAwaits where

import Arkham.Location.CardDefs.Import

abandonedCamp :: CardDef
abandonedCamp =
  locationWithUnrevealed
    "51050"
    "Diverging Path"
    [Dunwich, Woods]
    NoSymbol
    []
    "Abandoned Camp"
    [Dunwich, Woods]
    Droplet
    [Triangle, Trefoil]
    ReturnToWhereDoomAwaits

ascendingPathWarpedAndTwisted :: CardDef
ascendingPathWarpedAndTwisted =
  location
    "51049"
    ("Ascending Path" <:> "Warped and Twisted")
    [Dunwich, SentinelHill]
    Square
    [Triangle, Diamond, T, Equals, Moon, Trefoil]
    ReturnToWhereDoomAwaits

baseOfTheHillWarpedAndTwisted :: CardDef
baseOfTheHillWarpedAndTwisted =
  location
    "51048"
    ("Base of the Hill" <:> "Warped and Twisted")
    [Dunwich, SentinelHill]
    Triangle
    [Square, Plus, Squiggle, Hourglass, Droplet]
    ReturnToWhereDoomAwaits

fathomlessLake :: CardDef
fathomlessLake =
  locationWithUnrevealed
    "51051"
    "Altered Path"
    [Dunwich, Woods, Altered]
    NoSymbol
    []
    "Fathomless Lake"
    [Dunwich, Woods, Altered]
    Trefoil
    [Square, Droplet]
    ReturnToWhereDoomAwaits
