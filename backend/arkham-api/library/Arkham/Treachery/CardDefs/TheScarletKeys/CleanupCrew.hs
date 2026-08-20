module Arkham.Treachery.CardDefs.TheScarletKeys.CleanupCrew where

import Arkham.Treachery.CardDefs.Import

swiftRetreat :: CardDef
swiftRetreat =
  (treachery "09728" "Swift Retreat" CleanupCrew 2)
    { cdCardTraits = setFromList [Scheme]
    }
