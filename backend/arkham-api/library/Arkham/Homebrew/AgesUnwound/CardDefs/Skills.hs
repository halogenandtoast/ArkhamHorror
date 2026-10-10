module Arkham.Homebrew.AgesUnwound.CardDefs.Skills where

import Arkham.Homebrew.AgesUnwound.Sets qualified as Set
import Arkham.Skill.CardDefs.Import

monasticTraining :: CardDef
monasticTraining =
  (skill ":ages-unwound:141" "Monastic Training" [#wild, #wild, #wild] Neutral)
    { cdCardTraits = setFromList [Practiced, Expert]
    , -- "While Monastic Training is in your discard pile" — without this
      -- 'preloadDiscardEntities' never builds the entity and the card is dormant.
      cdOutOfPlayEffects = [InDiscardEffect]
    , cdLevel = Nothing
    , cdEncounterSet = Just Set.Missions
    , cdEncounterSetQuantity = Just 1
    }
