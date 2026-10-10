{- | The Masque of the Red Death's agenda deck.

The plague spreads by widening which chaos tokens pick up the chambers'
@[skull]@ effects: agenda 1 none, agenda 2 adds @[cultist]@ and @[tablet]@,
agenda 3 adds @[elder_thing]@ as well. Agenda 3's own advance kills everyone who
has not resigned, so the doom clock is the scenario's real timer.
-}
module Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Agendas where

import Arkham.Agenda.CardDefs.Import
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Sets qualified as Set

aNestOfVipers :: CardDef
aNestOfVipers =
  agenda ":the-masque-of-the-red-death:002" "A Nest of Vipers" 1 Set.TheMasqueOfTheRedDeath

underTheSkin :: CardDef
underTheSkin =
  agenda ":the-masque-of-the-red-death:003" "Under the Skin" 2 Set.TheMasqueOfTheRedDeath

diseaseVectors :: CardDef
diseaseVectors =
  agenda ":the-masque-of-the-red-death:004" "Disease Vectors" 3 Set.TheMasqueOfTheRedDeath
