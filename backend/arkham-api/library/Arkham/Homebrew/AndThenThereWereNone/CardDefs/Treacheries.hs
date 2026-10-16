module Arkham.Homebrew.AndThenThereWereNone.CardDefs.Treacheries where

import Arkham.Homebrew.AndThenThereWereNone.Sets qualified as Set
import Arkham.Treachery.CardDefs.Import

crypticMessage :: CardDef
crypticMessage =
  (treachery ":and-then-there-were-none:043" "Cryptic Message" Set.AndThenThereWereNone 1)
    { cdCardTraits = setFromList [Mystery, Omen]
    }

darkPresence :: CardDef
darkPresence =
  (treachery ":and-then-there-were-none:044" "Dark Presence" Set.AndThenThereWereNone 1)
    { cdCardTraits = setFromList [Curse, Omen]
    }

grimImplications :: CardDef
grimImplications =
  (treachery ":and-then-there-were-none:045" "Grim Implications" Set.AndThenThereWereNone 1)
    { cdCardTraits = setFromList [Mystery, Omen]
    }

hauntingNoises :: CardDef
hauntingNoises =
  (treachery ":and-then-there-were-none:046" "Haunting Noises" Set.AndThenThereWereNone 1)
    { cdCardTraits = singleton Terror
    }

lingeringHatred :: CardDef
lingeringHatred =
  (treachery ":and-then-there-were-none:047" "Lingering Hatred" Set.AndThenThereWereNone 1)
    { cdCardTraits = singleton Power
    }

phantomHands :: CardDef
phantomHands =
  (treachery ":and-then-there-were-none:048" "Phantom Hands" Set.AndThenThereWereNone 1)
    { cdCardTraits = singleton Hazard
    }

reenactment :: CardDef
reenactment =
  (treachery ":and-then-there-were-none:049" "Reenactment" Set.AndThenThereWereNone 1)
    { cdCardTraits = setFromList [Omen, Terror]
    }

thanatophobia :: CardDef
thanatophobia =
  (treachery ":and-then-there-were-none:050" "Thanatophobia" Set.AndThenThereWereNone 1)
    { cdCardTraits = setFromList [Terror, Madness]
    }

theLoomingDead :: CardDef
theLoomingDead =
  (treachery ":and-then-there-were-none:051" "The Looming Dead" Set.AndThenThereWereNone 1)
    { cdCardTraits = singleton Terror
    }

treacherousDarkness :: CardDef
treacherousDarkness =
  (treachery ":and-then-there-were-none:052" "Treacherous Darkness" Set.AndThenThereWereNone 1)
    { cdCardTraits = singleton Hazard
    }
