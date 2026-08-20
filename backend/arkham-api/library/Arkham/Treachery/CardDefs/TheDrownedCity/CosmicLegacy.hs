module Arkham.Treachery.CardDefs.TheDrownedCity.CosmicLegacy where

import Arkham.Treachery.CardDefs.Import

cunningMimicry :: CardDef
cunningMimicry =
  (treachery "11730" "Cunning Mimicry" CosmicLegacy 2) {cdCardTraits = setFromList [Scheme]}

eyeOfTheDeep :: CardDef
eyeOfTheDeep =
  (treachery "11729" "Eye of the Deep" CosmicLegacy 2) {cdCardTraits = setFromList [Power]}
