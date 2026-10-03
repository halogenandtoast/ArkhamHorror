module Arkham.Homebrew.ReturnToInnsmouth.CardDefs.Agendas where

import Arkham.Agenda.CardDefs.Import
import Arkham.Homebrew.ReturnToInnsmouth.Sets qualified as Set

-- | return_to_a_light_in_the_fog. Replaces Terror at Falcon Point (07235).
terrorAtFalconPointV2 :: CardDef
terrorAtFalconPointV2 =
  agenda ":return-to-innsmouth:040" "Terror at Falcon Point" 4 Set.ReturnToALightInTheFog

-- | return_to_into_the_maelstrom. Replaces Under the Surface (07312).
underTheSurfaceV2 :: CardDef
underTheSurfaceV2 =
  agenda ":return-to-innsmouth:049" "Under the Surface" 1 Set.ReturnToIntoTheMaelstrom

-- | return_to_into_the_maelstrom. Replaces Celestial Alignment (07313).
celestialAlignmentV2 :: CardDef
celestialAlignmentV2 =
  agenda ":return-to-innsmouth:050" "Celestial Alignment" 2 Set.ReturnToIntoTheMaelstrom
