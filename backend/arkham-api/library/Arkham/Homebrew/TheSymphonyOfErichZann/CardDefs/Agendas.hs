{- | The Symphony of Erich Zann's agenda deck.

Each agenda prints the maximum number of [[Music]] treacheries that may sit next
to the agenda deck at once (1, then 2, then 3); the cap itself is applied by
@Helpers.placeMusicTreachery@.

Coda Ultimatum is printed on the back of Opus Magnum, but is its own agenda here
at stage 4 -- see "Arkham.Homebrew.TheSymphonyOfErichZann.Agendas.CodaUltimatum".
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Agendas where

import Arkham.Agenda.CardDefs.Import
import Arkham.Homebrew.TheSymphonyOfErichZann.Sets qualified as Set

overture :: CardDef
overture = agenda ":the-symphony-of-erich-zann:002" "Overture" 1 Set.TheSymphonyOfErichZann

crescendo :: CardDef
crescendo = agenda ":the-symphony-of-erich-zann:003" "Crescendo" 2 Set.TheSymphonyOfErichZann

opusMagnum :: CardDef
opusMagnum =
  (agenda ":the-symphony-of-erich-zann:004" "Opus Magnum" 3 Set.TheSymphonyOfErichZann)
    { cdOtherSide = Just ":the-symphony-of-erich-zann:004b"
    }

codaUltimatum :: CardDef
codaUltimatum = agenda ":the-symphony-of-erich-zann:004b" "Coda Ultimatum" 4 Set.TheSymphonyOfErichZann
