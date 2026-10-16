module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Treacheries where

import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Sets qualified as Set
import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

aggressiveMutation :: CardDef
aggressiveMutation =
  ( treachery
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:021"
      "Aggressive Mutation"
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
      2
  )
    { cdCardTraits = singleton Power
    }

brutishCry :: CardDef
brutishCry =
  ( treachery
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:022"
      "Brutish Cry"
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
      2
  )
    { cdCardTraits = singleton Terror
    }

chemicalSpill :: CardDef
chemicalSpill =
  ( treachery
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:023"
      "Chemical Spill"
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
      2
  )
    { cdCardTraits = singleton Hazard
    , cdKeywords = singleton Keyword.Peril
    }

creepingGrowth :: CardDef
creepingGrowth =
  ( treachery
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:024"
      "Creeping Growth"
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
      3
  )
    { cdCardTraits = singleton Hazard
    }

enthrallingInterference :: CardDef
enthrallingInterference =
  ( treachery
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:025"
      "Enthralling Interference"
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
      3
  )
    { cdCardTraits = singleton Hex
    }

festeringEvil :: CardDef
festeringEvil =
  ( treachery
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:026"
      "Festering Evil"
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
      2
  )
    { cdCardTraits = singleton Scheme
    }

markOfTheGate :: CardDef
markOfTheGate =
  ( treachery
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:028"
      "Mark of the Gate"
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
      3
  )
    { cdCardTraits = singleton Hex
    }

supplyAndDemand :: CardDef
supplyAndDemand =
  ( treachery
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:032"
      "Supply and Demand"
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
      2
  )
    { cdCardTraits = singleton Scheme
    }

surgingFlesh :: CardDef
surgingFlesh =
  ( treachery
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:033"
      "Surging Flesh"
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
      3
  )
    { cdCardTraits = singleton Hazard
    }
