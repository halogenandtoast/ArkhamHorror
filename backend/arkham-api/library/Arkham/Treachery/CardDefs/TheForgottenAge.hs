module Arkham.Treachery.CardDefs.TheForgottenAge where

import Arkham.Treachery.CardDefs.Import

boughtInBlood :: CardDef
boughtInBlood =
  (weakness "04007" "Bought in Blood") {cdCardTraits = singleton Flaw}

callOfTheUnknown :: CardDef
callOfTheUnknown =
  (weakness "04009" "Call of the Unknown") {cdCardTraits = singleton Task}

caughtRedHanded :: CardDef
caughtRedHanded =
  (weakness "04012" "Caught Red-Handed") {cdCardTraits = singleton Blunder}

voiceOfTheMessenger :: CardDef
voiceOfTheMessenger =
  (weakness "04016" "Voice of the Messenger")
    { cdCardTraits = setFromList [Curse, Pact]
    }

thePriceOfFailure :: CardDef
thePriceOfFailure =
  (weakness "04039" "The Price of Failure") {cdCardTraits = singleton Pact}

doomed :: CardDef
doomed =
  (basicWeakness "04040" "Doomed")
    { cdCardTraits = singleton Curse
    , cdDeckRestrictions = [CampaignModeOnly]
    }

accursedFate :: CardDef
accursedFate =
  (weakness "04041" "Accursed Fate") {cdCardTraits = singleton Curse}

theBellTolls :: CardDef
theBellTolls =
  (weakness "04042" "The Bell Tolls") {cdCardTraits = singleton Curse}
