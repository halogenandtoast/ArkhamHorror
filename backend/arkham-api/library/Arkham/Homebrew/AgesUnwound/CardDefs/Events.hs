-- | The campaign's two printed events.
module Arkham.Homebrew.AgesUnwound.CardDefs.Events where

import Arkham.Event.Cards.Import
import Arkham.Homebrew.AgesUnwound.Sets qualified as Set

agencyStrikeTeam :: CardDef
agencyStrikeTeam =
  (event ":ages-unwound:140" "Agency Strike Team" 4 Neutral)
    { cdCardTraits = setFromList [Agency, Favor]
    , cdSkills = [#intellect, #intellect, #combat, #combat]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Nothing
    , cdEncounterSet = Just Set.Missions
    , cdEncounterSetQuantity = Just 1
    }

unstableEnergies :: CardDef
unstableEnergies =
  (event ":ages-unwound:235" "Unstable Energies" 1 Neutral)
    { cdCardTraits = singleton Spell
    , cdActions = #fight
    , cdLevel = Nothing
    , cdEncounterSet = Just Set.NightOfTheRitual
    , cdEncounterSetQuantity = Just 1
    }
