module Arkham.Homebrew.NightOfVespers.CardDefs.Agendas where

import Arkham.Agenda.CardDefs.Import
import Arkham.Homebrew.NightOfVespers.Sets qualified as Set

tooSwiftArrives :: CardDef
tooSwiftArrives = agenda ":night-of-vespers:002" "Too Swift Arrives..." 1 Set.TheEndOfSeasons

openSeason :: CardDef
openSeason = agenda ":night-of-vespers:003" "Open Season" 2 Set.TheEndOfSeasons

callingItANight :: CardDef
callingItANight = agenda ":night-of-vespers:004" "Calling It a Night" 3 Set.TheEndOfSeasons
