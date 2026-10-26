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

{- | Its __Forced__ fires from hand, so it needs 'InHandEffect' to exist as an
entity there at all -- without it the card has no in-hand entity, nothing asks it
for abilities, and the Forced never fires (Dark Memory is the same shape).
-}
unstableEnergies :: CardDef
unstableEnergies =
  (event ":ages-unwound:235" "Unstable Energies" 1 Neutral)
    { cdCardTraits = singleton Spell
    , cdActions = #fight
    , cdLevel = Nothing
    , cdOutOfPlayEffects = [InHandEffect]
    , cdEncounterSet = Just Set.NightOfTheRitual
    , cdEncounterSetQuantity = Just 1
    }
