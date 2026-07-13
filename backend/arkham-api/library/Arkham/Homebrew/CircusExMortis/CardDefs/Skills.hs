module Arkham.Homebrew.CircusExMortis.CardDefs.Skills where

import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Skill.CardDefs.Import

-- destiny_and_prophecy
invocationOfDiana :: CardDef
invocationOfDiana =
  (skill "z-circus-ex-mortis-242" "Invocation of Diana" [#wild] Neutral)
      { cdCardTraits = setFromList [Spirit]
      , cdLevel = Nothing
      , cdEncounterSet = Just Set.DestinyAndProphecy
      , cdEncounterSetQuantity = Just 1
      }
