module Arkham.Homebrew.TheInvisibleMan.CardDefs.Treacheries where

import Arkham.Homebrew.TheInvisibleMan.Sets qualified as Set
import Arkham.Treachery.CardDefs.Import

cleverSabotage :: CardDef
cleverSabotage =
  (treachery ":the-invisible-man:027" "Clever Sabotage" Set.TheInvisibleMan 2)
    { cdCardTraits = setFromList [Scheme, Hazard]
    }

doublingBack :: CardDef
doublingBack =
  (treachery ":the-invisible-man:028" "Doubling Back" Set.TheInvisibleMan 2)
    { cdCardTraits = singleton Scheme
    }

maliciousLaughter :: CardDef
maliciousLaughter =
  (treachery ":the-invisible-man:030" "Malicious Laughter" Set.TheInvisibleMan 3)
    { cdCardTraits = singleton Terror
    }

obviousSigns :: CardDef
obviousSigns =
  (treachery ":the-invisible-man:031" "Obvious Signs" Set.TheInvisibleMan 2)
    { cdCardTraits = setFromList [Scheme, Tactic]
    }

outOfNowhere :: CardDef
outOfNowhere =
  (treachery ":the-invisible-man:032" "Out of Nowhere" Set.TheInvisibleMan 3)
    { cdCardTraits = setFromList [Attack, Tactic]
    }

senselessMurder :: CardDef
senselessMurder =
  (treachery ":the-invisible-man:033" "Senseless Murder" Set.TheInvisibleMan 2)
    { cdCardTraits = setFromList [Scheme, Attack, Terror]
    }

subtleInterference :: CardDef
subtleInterference =
  (treachery ":the-invisible-man:036" "Subtle Interference" Set.TheInvisibleMan 2)
    { cdCardTraits = singleton Scheme
    }

unseenMovements :: CardDef
unseenMovements =
  (treachery ":the-invisible-man:038" "Unseen Movements" Set.TheInvisibleMan 3)
    { cdCardTraits = setFromList [Scheme, Tactic]
    }
