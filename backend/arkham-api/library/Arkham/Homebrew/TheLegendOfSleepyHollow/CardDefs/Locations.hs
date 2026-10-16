{- | The valley of Sleepy Hollow.

Every location is [[SleepyHollow]] and starts unrevealed on its own back; the
map never rewires, so each prints its connections once.
-}
module Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Locations where

import Arkham.Homebrew.TheLegendOfSleepyHollow.Sets qualified as Set
import Arkham.Homebrew.TheLegendOfSleepyHollow.Traits
import Arkham.Location.CardDefs.Import

hollow :: CardCode -> Name -> LocationSymbol -> [LocationSymbol] -> CardDef
hollow cardCode name symbol connections =
  location cardCode name [SleepyHollow] symbol connections Set.TheLegendOfSleepyHollow

vanTasselFarmstead :: CardDef
vanTasselFarmstead =
  hollow ":the-legend-of-sleepy-hollow:006" "Van Tassel Farmstead" Square [T]

majorAndresTree :: CardDef
majorAndresTree =
  hollow ":the-legend-of-sleepy-hollow:007" "Major Andre's Tree" T [Square, Equals]

wileysSwamp :: CardDef
wileysSwamp =
  hollow ":the-legend-of-sleepy-hollow:008" "Wiley's Swamp" Equals [T, Plus, Squiggle, Trefoil]

derelictSchoolhouse :: CardDef
derelictSchoolhouse =
  hollow ":the-legend-of-sleepy-hollow:009" "Derelict Schoolhouse" Squiggle [Equals, Trefoil]

oldDutchChurch :: CardDef
oldDutchChurch =
  victory 1
    $ (hollow ":the-legend-of-sleepy-hollow:010" "Old Dutch Church" Plus [Equals])
      { cdCardTraits = setFromList [SleepyHollow, Blessed]
      }

whitePlains :: CardDef
whitePlains =
  victory 1 $ hollow ":the-legend-of-sleepy-hollow:011" "White Plains" Trefoil [Equals, Squiggle]
