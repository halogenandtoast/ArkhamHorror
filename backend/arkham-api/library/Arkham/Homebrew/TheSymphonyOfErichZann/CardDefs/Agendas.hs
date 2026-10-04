{- | The Symphony of Erich Zann's agenda deck.

Each agenda prints the maximum number of [[Music]] treacheries that may sit next
to the agenda deck at once (1, then 2, then 3); the cap itself is applied by
@Helpers.placeMusicTreachery@. Agenda 3b, Coda Ultimatum, becomes both the
current act and the current agenda.
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Agendas where

import Arkham.Agenda.CardDefs.Import
import Arkham.Homebrew.TheSymphonyOfErichZann.Sets qualified as Set

overture :: CardDef
overture = agenda ":the-symphony-of-erich-zann:002" "Overture" 1 Set.TheSymphonyOfErichZann

crescendo :: CardDef
crescendo = agenda ":the-symphony-of-erich-zann:003" "Crescendo" 2 Set.TheSymphonyOfErichZann

opusMagnum :: CardDef
opusMagnum = agenda ":the-symphony-of-erich-zann:004" "Opus Magnum" 3 Set.TheSymphonyOfErichZann
