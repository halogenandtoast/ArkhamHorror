module Arkham.Treachery.CardDefs.TheScarletKeys.Globetrotting where

import Arkham.Treachery.CardDefs.Import

paradimensionalUnderstanding :: CardDef
paradimensionalUnderstanding =
  (weakness "09767" "Paradimensional Understanding")
    { cdCardTraits = setFromList [Madness, Paradox]
    , cdEncounterSet = Just Globetrotting
    , cdEncounterSetQuantity = Just 4
    }
