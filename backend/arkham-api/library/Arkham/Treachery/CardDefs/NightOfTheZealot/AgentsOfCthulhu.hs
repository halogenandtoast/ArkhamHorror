module Arkham.Treachery.CardDefs.NightOfTheZealot.AgentsOfCthulhu where

import Arkham.Treachery.CardDefs.Import

dreamsOfRlyeh :: CardDef
dreamsOfRlyeh =
  (treachery "01182" "Dreams of R'lyeh" AgentsOfCthulhu 2)
    { cdCardTraits = setFromList [Omen]
    }
