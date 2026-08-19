module Arkham.Agenda.CardDefs.TheForgottenAge.ShatteredAeons where

import Arkham.Agenda.CardDefs.Import

threadsOfTime :: CardDef
threadsOfTime = (agenda "04315" "Threads of Time" 1 ShatteredAeons) {cdVengeancePoints = Just 1}

pendulousThreads :: CardDef
pendulousThreads = agenda "04316" "Pendulous Threads" 2 ShatteredAeons

snappedThreads :: CardDef
snappedThreads = agenda "04317" "Snapped Threads" 3 ShatteredAeons
