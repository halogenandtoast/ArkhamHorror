{- | Consternation on the Constellation's agenda deck.

Agenda 3 is a branch, not a card: advancing act 2 puts "Punish the Interlopers"
out, advancing agenda 2 puts "Summon Those Below" out instead, and only one of
the two is ever used. Both print the same @b@ side -- the ship lists, a location
floods, and the agenda flips back to 3a -- so the deck never runs out.
-}
module Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Agendas where

import Arkham.Agenda.CardDefs.Import
import Arkham.Homebrew.ConsternationOnTheConstellation.Sets qualified as Set

captured :: CardDef
captured =
  agenda ":consternation-on-the-constellation:002" "Captured!" 1 Set.ConsternationOnTheConstellation

searchTheShip :: CardDef
searchTheShip =
  agenda
    ":consternation-on-the-constellation:003"
    "Search the Ship"
    2
    Set.ConsternationOnTheConstellation

-- | Agenda 3a when act 2 advanced first. The investigators hold the tablet.
punishTheInterlopers :: CardDef
punishTheInterlopers =
  agenda
    ":consternation-on-the-constellation:004"
    "Punish the Interlopers"
    3
    Set.ConsternationOnTheConstellation

-- | Agenda 3a when agenda 2 advanced first. The cult holds the tablet.
summonThoseBelow :: CardDef
summonThoseBelow =
  agenda
    ":consternation-on-the-constellation:005"
    "Summon Those Below"
    3
    Set.ConsternationOnTheConstellation
