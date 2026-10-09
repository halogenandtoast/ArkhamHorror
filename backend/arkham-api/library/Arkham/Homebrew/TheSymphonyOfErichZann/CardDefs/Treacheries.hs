{- | The Symphony of Erich Zann's treacheries.

The six [[Music]] treacheries are the scenario's engine: instead of being
discarded on resolution they are put into play next to the agenda deck, where
they stay until the agenda's maximum pushes the earliest one out. Four of them
carry an instrument trait that unlocks the matching Musician enemy.
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries where

import Arkham.Homebrew.TheSymphonyOfErichZann.Sets qualified as Set
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits hiding (pattern Piano, pattern String)
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits qualified as T
import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

{- | Dealt to each Performer investigator at setup and by Investigator Defeat.
Four copies travel with the encounter set; none are shuffled into the deck.
-}
stuckInYourHead :: CardDef
stuckInYourHead =
  (weakness ":the-symphony-of-erich-zann:028" "Stuck in Your Head")
    { cdCardTraits = singleton Madness
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdEncounterSet = Just Set.TheSymphonyOfErichZann
    , cdEncounterSetQuantity = Just 4
    }

deafeningBrass :: CardDef
deafeningBrass =
  (treachery ":the-symphony-of-erich-zann:029" "Deafening Brass" Set.TheSymphonyOfErichZann 2)
    { cdCardTraits = setFromList [Music, Brass]
    }

diesIrae :: CardDef
diesIrae =
  (treachery ":the-symphony-of-erich-zann:030" "Dies Irae" Set.TheSymphonyOfErichZann 2)
    { cdCardTraits = setFromList [Music, Terror]
    , cdKeywords = singleton Keyword.Peril
    }

endlessEcho :: CardDef
endlessEcho =
  (treachery ":the-symphony-of-erich-zann:032" "Endless Echo" Set.TheSymphonyOfErichZann 2)
    { cdCardTraits = singleton Hazard
    }

etherealMelody :: CardDef
etherealMelody =
  (treachery ":the-symphony-of-erich-zann:033" "Ethereal Melody" Set.TheSymphonyOfErichZann 2)
    { cdCardTraits = setFromList [Music, T.Piano]
    }

heardBySomething :: CardDef
heardBySomething =
  (treachery ":the-symphony-of-erich-zann:034" "Heard by Something" Set.TheSymphonyOfErichZann 3)
    { cdCardTraits = singleton Omen
    }

hissingNoise :: CardDef
hissingNoise =
  (treachery ":the-symphony-of-erich-zann:035" "Hissing Noise" Set.TheSymphonyOfErichZann 2)
    { cdCardTraits = singleton Terror
    }

overwhelm :: CardDef
overwhelm =
  (treachery ":the-symphony-of-erich-zann:037" "Overwhelm" Set.TheSymphonyOfErichZann 2)
    { cdCardTraits = singleton Terror
    }

rhythmFromBeyond :: CardDef
rhythmFromBeyond =
  (treachery ":the-symphony-of-erich-zann:038" "Rhythm from Beyond" Set.TheSymphonyOfErichZann 2)
    { cdCardTraits = setFromList [Music, Percussion]
    }

shriekingViolin :: CardDef
shriekingViolin =
  (treachery ":the-symphony-of-erich-zann:039" "Shrieking Violin" Set.TheSymphonyOfErichZann 2)
    { cdCardTraits = setFromList [Music, T.String]
    }

waltzOfTheSpheres :: CardDef
waltzOfTheSpheres =
  (treachery ":the-symphony-of-erich-zann:041" "Waltz of the Spheres" Set.TheSymphonyOfErichZann 3)
    { cdCardTraits = singleton Power
    }

turnaround :: CardDef
turnaround =
  surge
    $ (treachery ":the-symphony-of-erich-zann:043" "Turnaround" Set.TheSymphonyOfErichZann 2)
      { cdCardTraits = singleton Tactic
      }

{- | The back of the Beyond the Curtain story card. It attaches to a location
rather than being one: the card was reworked from a location into a treachery.
-}
theWindowToNothingness :: CardDef
theWindowToNothingness =
  ( treachery
      ":the-symphony-of-erich-zann:008b"
      ("The Window to Nothingness" <:> "Impenetrable Silence")
      Set.TheSymphonyOfErichZann
      1
  )
    { cdCardTraits = singleton Extradimensional
    , cdOtherSide = Just ":the-symphony-of-erich-zann:008"
    }
