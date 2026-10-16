{- | Soldier Island.

Terrace is the [[Central]] hub and touches every other room; the five outer
locations each hold two [[Victim]] story assets and two facedown [[Method]]
cards at setup. Every connection is printed, so the map never rewires.
-}
module Arkham.Homebrew.AndThenThereWereNone.CardDefs.Locations where

import Arkham.Homebrew.AndThenThereWereNone.Sets qualified as Set
import Arkham.Homebrew.AndThenThereWereNone.Traits (pattern SoldierIsland)
import Arkham.Location.CardDefs.Import

soldierIsland :: CardCode -> Name -> LocationSymbol -> [LocationSymbol] -> CardDef
soldierIsland cardCode name symbol connections =
  location cardCode name [SoldierIsland] symbol connections Set.AndThenThereWereNone

terrace :: CardDef
terrace =
  (soldierIsland ":and-then-there-were-none:005" "Terrace" Plus [Triangle, T, Circle, Star, Moon])
    { cdCardTraits = setFromList [SoldierIsland, Central]
    }

beach :: CardDef
beach = soldierIsland ":and-then-there-were-none:006" "Beach" Triangle [Plus, T, Circle]

bedrooms :: CardDef
bedrooms = soldierIsland ":and-then-there-were-none:007" "Bedrooms" Moon [Plus, Star, Circle]

diningRoom :: CardDef
diningRoom =
  soldierIsland ":and-then-there-were-none:008" "Dining Room" Star [Plus, T, Circle, Moon]

sunroom :: CardDef
sunroom =
  soldierIsland ":and-then-there-were-none:009" "Sunroom" Circle [Plus, Triangle, Star, Moon]

woodShed :: CardDef
woodShed = soldierIsland ":and-then-there-were-none:010" "Wood Shed" T [Plus, Triangle, Star]
