module Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Treacheries where

import Arkham.Homebrew.TheLegendOfSleepyHollow.Sets qualified as Set
import Arkham.Treachery.CardDefs.Import

accursedHollow :: CardDef
accursedHollow =
  (treachery ":the-legend-of-sleepy-hollow:015" "Accursed Hollow" Set.TheLegendOfSleepyHollow 2)
    { cdCardTraits = singleton Curse
    }

bewitched :: CardDef
bewitched =
  (treachery ":the-legend-of-sleepy-hollow:016" "Bewitched" Set.TheLegendOfSleepyHollow 4)
    { cdCardTraits = setFromList [Hex, Curse]
    }

burgeoningEvil :: CardDef
burgeoningEvil =
  (treachery ":the-legend-of-sleepy-hollow:017" "Burgeoning Evil" Set.TheLegendOfSleepyHollow 3)
    { cdCardTraits = setFromList [Power, Hex]
    }

hurledPumpkin :: CardDef
hurledPumpkin =
  (treachery ":the-legend-of-sleepy-hollow:021" "Hurled Pumpkin" Set.TheLegendOfSleepyHollow 3)
    { cdCardTraits = singleton Scheme
    }

loomingSpirit :: CardDef
loomingSpirit =
  (treachery ":the-legend-of-sleepy-hollow:022" "Looming Spirit" Set.TheLegendOfSleepyHollow 2)
    { cdCardTraits = setFromList [Scheme, Curse]
    }

markedByTheHorseman :: CardDef
markedByTheHorseman =
  ( treachery
      ":the-legend-of-sleepy-hollow:023"
      "Marked by the Horseman"
      Set.TheLegendOfSleepyHollow
      3
  )
    { cdCardTraits = singleton Scheme
    }

quickeningRitual :: CardDef
quickeningRitual =
  (treachery ":the-legend-of-sleepy-hollow:024" "Quickening Ritual" Set.TheLegendOfSleepyHollow 4)
    { cdCardTraits = setFromList [Hex, Curse]
    }

whispersOnTheWind :: CardDef
whispersOnTheWind =
  (treachery ":the-legend-of-sleepy-hollow:026" "Whispers on the Wind" Set.TheLegendOfSleepyHollow 3)
    { cdCardTraits = setFromList [Hex, Terror]
    }
