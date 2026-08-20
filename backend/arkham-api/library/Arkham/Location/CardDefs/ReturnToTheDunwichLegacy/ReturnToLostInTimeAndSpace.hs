module Arkham.Location.CardDefs.ReturnToTheDunwichLegacy.ReturnToLostInTimeAndSpace where

import Arkham.Location.CardDefs.Import

indecipherableStairs :: CardDef
indecipherableStairs =
  singleSided
    $ revelation
    $ location
      "51058"
      "Indecipherable Stairs"
      [Otherworld, Extradimensional]
      Triangle
      [Square, Equals]
      ReturnToLostInTimeAndSpace

realmsBeyondAllInOne :: CardDef
realmsBeyondAllInOne =
  singleSided
    $ location
      "51057"
      ("Realms Beyond" <:> "All-In-One")
      [Otherworld]
      Droplet
      [Circle]
      ReturnToLostInTimeAndSpace

toweringLuminosity :: CardDef
toweringLuminosity =
  singleSided
    $ revelation
    $ location
      "51059"
      "Towering Luminosity"
      [Otherworld, Extradimensional]
      Diamond
      [Square, Equals]
      ReturnToLostInTimeAndSpace

unstableVortex :: CardDef
unstableVortex =
  singleSided
    $ revelation
    $ location
      "51060"
      "Unstable Vortex"
      [Otherworld, Extradimensional]
      Equals
      [Square, Moon, Plus, Squiggle]
      ReturnToLostInTimeAndSpace
