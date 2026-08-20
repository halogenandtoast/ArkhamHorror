module Arkham.Treachery.CardDefs.ChildrenOfBlood.Infected where

import Arkham.Treachery.CardDefs.Import

theBloodBlight :: CardDef
theBloodBlight =
  (weakness "13118" "The Blood Blight")
    { cdCardTraits = setFromList [Curse, Blight]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdPermanent = True
    , cdEncounterSet = Just Infected
    , cdEncounterSetQuantity = Just 4
    }
