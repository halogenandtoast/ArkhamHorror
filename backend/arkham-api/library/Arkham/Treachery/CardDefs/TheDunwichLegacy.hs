module Arkham.Treachery.CardDefs.TheDunwichLegacy where

import Arkham.Treachery.CardDefs.Import

smiteTheWicked :: CardDef
smiteTheWicked =
  signature "02001"
    $ (weakness "02007" "Smite the Wicked") {cdCardTraits = setFromList [Task]}

rexsCurse :: CardDef
rexsCurse =
  signature "02002"
    $ (weakness "02009" "Rex's Curse") {cdCardTraits = setFromList [Curse]}

searchingForIzzie :: CardDef
searchingForIzzie =
  signature "02003"
    $ (weakness "02011" "Searching for Izzie") {cdCardTraits = setFromList [Task]}

finalRhapsody :: CardDef
finalRhapsody =
  signature "02004"
    $ (weakness "02013" "Final Rhapsody") {cdCardTraits = setFromList [Endtimes]}

wrackedByNightmares :: CardDef
wrackedByNightmares =
  signature "02005"
    $ (weakness "02015" "Wracked by Nightmares")
      { cdCardTraits = setFromList [Madness]
      }

indebted :: CardDef
indebted =
  (basicWeakness "02037" "Indebted")
    { cdCardTraits = singleton Flaw
    , cdPermanent = True
    , cdTags = [setupOnlyTag]
    }

internalInjury :: CardDef
internalInjury =
  (basicWeakness "02038" "Internal Injury") {cdCardTraits = singleton Injury}

chronophobia :: CardDef
chronophobia =
  (basicWeakness "02039" "Chronophobia") {cdCardTraits = singleton Madness}
