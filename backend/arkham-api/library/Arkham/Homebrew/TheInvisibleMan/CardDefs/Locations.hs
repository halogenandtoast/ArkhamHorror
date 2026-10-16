{- | The wooded roads south of Arkham. Every location is [[Backwoods]] and
starts unrevealed on its own back; the map never rewires, so each prints its
connections once.
-}
module Arkham.Homebrew.TheInvisibleMan.CardDefs.Locations where

import Arkham.Homebrew.TheInvisibleMan.Sets qualified as Set
import Arkham.Homebrew.TheInvisibleMan.Traits
import Arkham.Location.CardDefs.Import

backwoods :: CardCode -> Name -> LocationSymbol -> [LocationSymbol] -> CardDef
backwoods cardCode name symbol connections =
  location cardCode name [Backwoods] symbol connections Set.TheInvisibleMan

countryRoad :: CardDef
countryRoad =
  backwoods ":the-invisible-man:009" "Country Road" Circle [Square, Heart, Diamond, Triangle]

serviceStation :: CardDef
serviceStation =
  backwoods ":the-invisible-man:010" "Service Station" Heart [Moon, Square, Circle, Diamond]

runDownCabin :: CardDef
runDownCabin =
  backwoods ":the-invisible-man:011" "Run-Down Cabin" Square [Star, Moon, Heart, Circle]

moonshinersShack :: CardDef
moonshinersShack =
  victory 1
    $ backwoods ":the-invisible-man:012" "Moonshiners' Shack" Moon [Triangle, Star, Square, Heart]

sparseWoodlands :: CardDef
sparseWoodlands =
  backwoods ":the-invisible-man:013" "Sparse Woodlands" Star [Diamond, Triangle, Moon, Square]

meltwaterStream :: CardDef
meltwaterStream =
  backwoods ":the-invisible-man:014" "Meltwater Stream" Triangle [Circle, Diamond, Star, Moon]

lonelyFarmstead :: CardDef
lonelyFarmstead =
  backwoods ":the-invisible-man:015" "Lonely Farmstead" Diamond [Heart, Circle, Triangle, Star]
