module Arkham.Skill.CardDefs.CircusExMortis where

import Arkham.EncounterSet (EncounterSet (CircusExMortisDestinyAndProphecy))
import Arkham.Skill.CardDefs.Import

-- destiny_and_prophecy
invocationOfDianaCircusExMortis :: CardDef
invocationOfDianaCircusExMortis =
  (skill "z-circus-ex-mortis-242" "Invocation of Diana" [#wild] Neutral)
      { cdCardTraits = setFromList [Spirit]
      , cdLevel = Nothing
      , cdEncounterSet = Just CircusExMortisDestinyAndProphecy
      , cdEncounterSetQuantity = Just 1
      }
