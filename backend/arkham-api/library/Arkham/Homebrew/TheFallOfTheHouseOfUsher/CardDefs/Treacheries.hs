module Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Treacheries where

import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Sets qualified as Set
import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

auditoryHallucinations :: CardDef
auditoryHallucinations =
  ( treachery
      ":the-fall-of-the-house-of-usher:016"
      "Auditory Hallucinations"
      Set.TheFallOfTheHouseOfUsher
      2
  )
    { cdCardTraits = setFromList [Madness, Terror]
    }

collapsingRuin :: CardDef
collapsingRuin =
  (treachery ":the-fall-of-the-house-of-usher:018" "Collapsing Ruin" Set.TheFallOfTheHouseOfUsher 3)
    { cdCardTraits = singleton Hazard
    }

decayedLegacy :: CardDef
decayedLegacy =
  (treachery ":the-fall-of-the-house-of-usher:021" "Decayed Legacy" Set.TheFallOfTheHouseOfUsher 2)
    { cdCardTraits = singleton Curse
    }

desolateHeritage :: CardDef
desolateHeritage =
  (treachery ":the-fall-of-the-house-of-usher:023" "Desolate Heritage" Set.TheFallOfTheHouseOfUsher 2)
    { cdCardTraits = singleton Curse
    }

enervatingTouch :: CardDef
enervatingTouch =
  (treachery ":the-fall-of-the-house-of-usher:025" "Enervating Touch" Set.TheFallOfTheHouseOfUsher 3)
    { cdCardTraits = setFromList [Geist, Hazard]
    }

enshroudedHost :: CardDef
enshroudedHost =
  (treachery ":the-fall-of-the-house-of-usher:028" "Enshrouded Host" Set.TheFallOfTheHouseOfUsher 2)
    { cdCardTraits = setFromList [Geist, Mystery]
    }

morbidAcuteness :: CardDef
morbidAcuteness =
  (treachery ":the-fall-of-the-house-of-usher:033" "Morbid Acuteness" Set.TheFallOfTheHouseOfUsher 3)
    { cdCardTraits = setFromList [Curse, Madness]
    }

nervousAffliction :: CardDef
nervousAffliction =
  (treachery ":the-fall-of-the-house-of-usher:036" "Nervous Affliction" Set.TheFallOfTheHouseOfUsher 3)
    { cdCardTraits = setFromList [Madness, Terror]
    }

pestilentVapor :: CardDef
pestilentVapor =
  (treachery ":the-fall-of-the-house-of-usher:039" "Pestilent Vapor" Set.TheFallOfTheHouseOfUsher 3)
    { cdCardTraits = singleton Curse
    , cdKeywords = singleton Keyword.Peril
    }
