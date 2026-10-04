{- | Against the Wendigo's agenda deck.

Agenda 2's @b@ side is not an agenda at all -- the card flips into the Bestial
Creature enemy (see @CardDefs.Enemies.bestialCreature@), which is why advancing
it removes the agenda from the deck instead of continuing it.
-}
module Arkham.Homebrew.AgainstTheWendigo.CardDefs.Agendas where

import Arkham.Agenda.CardDefs.Import
import Arkham.Homebrew.AgainstTheWendigo.Sets qualified as Set

aDarkAndDisturbingValley :: CardDef
aDarkAndDisturbingValley =
  agenda ":against-the-wendigo:002" "A Dark and Disturbing Valley" 1 Set.HanninahValley

somethingDarkIsComing :: CardDef
somethingDarkIsComing =
  ( agenda ":against-the-wendigo:003" "Something Dark Is Coming" 2 Set.HanninahValley
  )
    { cdOtherSide = Just ":against-the-wendigo:003b"
    }

theWendigoHuntsYou :: CardDef
theWendigoHuntsYou =
  agenda ":against-the-wendigo:004" "The Wendigo Hunts You" 3 Set.HanninahValley
