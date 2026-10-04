{- | The Symphony of Erich Zann's story cards.

Four of these are the backs of the Musician enemies -- a Musician that is
parleyed with successfully flips to its Muse, which banks the victory point and
offers its instrument as a reward. Beyond the Curtain is the only one set aside
on its own; it flips into The Window to Nothingness, a location.
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories where

import Arkham.Card.CardDef
import Arkham.Homebrew.TheSymphonyOfErichZann.Sets qualified as Set
import Arkham.Prelude
import Arkham.Story.CardDefs.Base

beyondTheCurtain :: CardDef
beyondTheCurtain =
  doubleSided
    $ story ":the-symphony-of-erich-zann:008" "Beyond the Curtain" Set.TheSymphonyOfErichZann

trumpetersMuse :: CardDef
trumpetersMuse =
  (story ":the-symphony-of-erich-zann:020b" "Trumpeter's Muse" Set.TheSymphonyOfErichZann)
    { cdOtherSide = Just ":the-symphony-of-erich-zann:020"
    }

pianistsMuse :: CardDef
pianistsMuse =
  (story ":the-symphony-of-erich-zann:021b" "Pianist's Muse" Set.TheSymphonyOfErichZann)
    { cdOtherSide = Just ":the-symphony-of-erich-zann:021"
    }

violinistsMuse :: CardDef
violinistsMuse =
  (story ":the-symphony-of-erich-zann:022b" "Violinist's Muse" Set.TheSymphonyOfErichZann)
    { cdOtherSide = Just ":the-symphony-of-erich-zann:022"
    }

percussionistsMuse :: CardDef
percussionistsMuse =
  (story ":the-symphony-of-erich-zann:023b" "Percussionist's Muse" Set.TheSymphonyOfErichZann)
    { cdOtherSide = Just ":the-symphony-of-erich-zann:023"
    }

-- | The Piano's story side. Distinct card code from the enemy's Pianist's Muse.
thePianosMuse :: CardDef
thePianosMuse =
  (story ":the-symphony-of-erich-zann:045b" "Pianist's Muse" Set.TheSymphonyOfErichZann)
    { cdOtherSide = Just ":the-symphony-of-erich-zann:045"
    , cdArt = ":the-symphony-of-erich-zann:045b"
    }
