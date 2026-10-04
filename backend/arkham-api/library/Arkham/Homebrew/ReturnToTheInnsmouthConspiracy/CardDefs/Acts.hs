module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Acts where

import Arkham.Act.CardDefs.Import
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Set

-- | return_to_the_pit_of_despair. Replaces The Pit (07045).
thePitV2 :: CardDef
thePitV2 = act ":return-to-the-innsmouth-conspiracy:019" "The Pit" 1 Set.ReturnToThePitOfDespair

-- | return_to_the_vanishing_of_elina_harper. Replaces The Search for Agent Harper (07060).
theSearchForAgentHarperV2 :: CardDef
theSearchForAgentHarperV2 =
  act
    ":return-to-the-innsmouth-conspiracy:023"
    "The Search for Agent Harper"
    1
    Set.ReturnToTheVanishingOfElinaHarper

-- | return_to_in_too_deep. Replaces Through the Labyrinth (07128).
throughTheLabyrinthV2 :: CardDef
throughTheLabyrinthV2 =
  act ":return-to-the-innsmouth-conspiracy:029" "Through the Labyrinth" 1 Set.ReturnToInTooDeep

-- | return_to_the_lair_of_dagon. Replaces The Second Oath (07281).
theSecondOathV2 :: CardDef
theSecondOathV2 = act ":return-to-the-innsmouth-conspiracy:046" "The Second Oath" 2 Set.ReturnToTheLairOfDagon

-- | return_to_into_the_maelstrom. Replaces Back into the Depths (07315).
backIntoTheDepthsV2 :: CardDef
backIntoTheDepthsV2 =
  act ":return-to-the-innsmouth-conspiracy:051" "Back into the Depths" 1 Set.ReturnToIntoTheMaelstrom
