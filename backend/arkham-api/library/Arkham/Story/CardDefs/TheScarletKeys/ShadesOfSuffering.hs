module Arkham.Story.CardDefs.TheScarletKeys.ShadesOfSuffering where

import Arkham.Story.CardDefs.Import

aLostMemento :: CardDef
aLostMemento =
  (story "09675b" "A Lost Memento" ShadesOfSuffering & otherSideIs "09675a")
    { cdVictoryPoints = Just 1
    }

exhumeTheBones :: CardDef
exhumeTheBones =
  (story "09675d" "Exhume the Bones" ShadesOfSuffering & otherSideIs "09675c")
    { cdVictoryPoints = Just 1
    }

familialPain :: CardDef
familialPain =
  (story "09676d" "Familial Pain" ShadesOfSuffering & otherSideIs "09676c") {cdVictoryPoints = Just 1}

playfulShadows :: CardDef
playfulShadows =
  (story "09674b" "Playful Shadows" ShadesOfSuffering & otherSideIs "09674a")
    { cdVictoryPoints = Just 1
    }

sympathyPain :: CardDef
sympathyPain =
  (story "09676b" "Sympathy Pain" ShadesOfSuffering & otherSideIs "09676a") {cdVictoryPoints = Just 1}

timorousShadows :: CardDef
timorousShadows =
  (story "09674d" "Timorous Shadows" ShadesOfSuffering & otherSideIs "09674c")
    { cdVictoryPoints = Just 1
    }
