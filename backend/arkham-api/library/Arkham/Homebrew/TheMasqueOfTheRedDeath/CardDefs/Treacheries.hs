{- | The Masque of the Red Death's treacheries.

Several of them stay on the table rather than resolving and going away: Festering
Horror and Frenetic Dance sit next to the agenda deck, Silent Scrutiny attaches
to a location and shuts off its clues, and Morbid Visions, Shadow of Death and
Violent Crowd sit in a threat area granting their owner's *location* an extra
@[skull]@ effect -- which is what the chambers, the agendas and Maddened Reveler
all count.
-}
module Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Treacheries where

import Arkham.Homebrew.TheMasqueOfTheRedDeath.Sets qualified as Set
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Traits (pattern Disease)
import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

debauchery :: CardDef
debauchery =
  (treachery ":the-masque-of-the-red-death:035" "Debauchery" Set.TheMasqueOfTheRedDeath 3)
    { cdCardTraits = setFromList [Scheme, Omen]
    , cdKeywords = singleton Keyword.Peril
    }

festeringHorror :: CardDef
festeringHorror =
  (treachery ":the-masque-of-the-red-death:038" "Festering Horror" Set.TheMasqueOfTheRedDeath 2)
    { cdCardTraits = singleton Terror
    }

freneticDance :: CardDef
freneticDance =
  (treachery ":the-masque-of-the-red-death:040" "Frenetic Dance" Set.TheMasqueOfTheRedDeath 2)
    { cdCardTraits = singleton Scheme
    }

morbidVisions :: CardDef
morbidVisions =
  (treachery ":the-masque-of-the-red-death:042" "Morbid Visions" Set.TheMasqueOfTheRedDeath 3)
    { cdCardTraits = setFromList [Omen, Terror]
    }

shadowOfDeath :: CardDef
shadowOfDeath =
  (treachery ":the-masque-of-the-red-death:045" "Shadow of Death" Set.TheMasqueOfTheRedDeath 2)
    { cdCardTraits = singleton Omen
    }

silentScrutiny :: CardDef
silentScrutiny =
  (treachery ":the-masque-of-the-red-death:047" "Silent Scrutiny" Set.TheMasqueOfTheRedDeath 2)
    { cdCardTraits = singleton Scheme
    }

suddenSymptoms :: CardDef
suddenSymptoms =
  (treachery ":the-masque-of-the-red-death:049" "Sudden Symptoms" Set.TheMasqueOfTheRedDeath 3)
    { cdCardTraits = singleton Disease
    }

violentCrowd :: CardDef
violentCrowd =
  (treachery ":the-masque-of-the-red-death:052" "Violent Crowd" Set.TheMasqueOfTheRedDeath 3)
    { cdCardTraits = setFromList [Attack, Hazard]
    }
