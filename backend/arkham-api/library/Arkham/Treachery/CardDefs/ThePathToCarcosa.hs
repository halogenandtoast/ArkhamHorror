module Arkham.Treachery.CardDefs.ThePathToCarcosa where

import Arkham.Treachery.CardDefs.Import

shellShock :: CardDef
shellShock =
  (weakness "03008" "Shell Shock") {cdCardTraits = setFromList [Flaw]}

starsOfHyades :: CardDef
starsOfHyades =
  (weakness "03013" "Stars of Hyades") {cdCardTraits = setFromList [Curse]}

angeredSpirits :: CardDef
angeredSpirits =
  (weakness "03015" "Angered Spirits") {cdCardTraits = singleton Task}

crisisOfIdentity :: CardDef
crisisOfIdentity =
  (weakness "03019" "Crisis of Identity") {cdCardTraits = singleton Madness}

overzealous :: CardDef
overzealous =
  (basicWeakness "03040" "Overzealous")
    { cdCardTraits = singleton Flaw
    , cdAlternateCardCodes = ["12100"]
    }

drawingTheSign :: CardDef
drawingTheSign =
  (basicWeakness "03041" "Drawing the Sign")
    { cdCardTraits = setFromList [Pact, Madness]
    }
