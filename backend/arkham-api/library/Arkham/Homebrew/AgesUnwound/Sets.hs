module Arkham.Homebrew.AgesUnwound.Sets (
  module Arkham.EncounterSet,
  pattern AWorldTornDown,
  pattern AWorldTornDownAgain,
  pattern AYearToPlan,
  pattern AgentsOfChronos,
  pattern Missions,
  pattern Myriad,
  pattern NightOfFire,
  pattern NightOfTheRitual,
  pattern Nyctophobia,
  pattern Paradox,
  pattern ShiftingReality,
  pattern TheMyriadGentleman,
  pattern Thugs,
  pattern TimeRunsOut,
  pattern UnleashedChaos,
  pattern UnravellingYears,
  pattern Unstuck,
) where

import Arkham.EncounterSet

-- | Scenario III.
pattern AWorldTornDown :: EncounterSet
pattern AWorldTornDown = Homebrew ":ages-unwound:a_world_torn_down"

-- | Scenario VI.
pattern AWorldTornDownAgain :: EncounterSet
pattern AWorldTornDownAgain = Homebrew ":ages-unwound:a_world_torn_down_again"

-- | Scenario V.
pattern AYearToPlan :: EncounterSet
pattern AYearToPlan = Homebrew ":ages-unwound:a_year_to_plan"

-- | Shared; the guide calls this set "Agents of Aforgomon".
pattern AgentsOfChronos :: EncounterSet
pattern AgentsOfChronos = Homebrew ":ages-unwound:agents_of_chronos"

-- | gathered only by Scenario V, but large and self-contained.
pattern Missions :: EncounterSet
pattern Missions = Homebrew ":ages-unwound:missions"

-- | Shared.
pattern Myriad :: EncounterSet
pattern Myriad = Homebrew ":ages-unwound:myriad"

-- | Scenario I.
pattern NightOfFire :: EncounterSet
pattern NightOfFire = Homebrew ":ages-unwound:night_of_fire"

-- | Shared by Scenarios III and VI.
pattern NightOfTheRitual :: EncounterSet
pattern NightOfTheRitual = Homebrew ":ages-unwound:night_of_the_ritual"

-- | Shared.
pattern Nyctophobia :: EncounterSet
pattern Nyctophobia = Homebrew ":ages-unwound:nyctophobia"

-- | Shared.
pattern Paradox :: EncounterSet
pattern Paradox = Homebrew ":ages-unwound:paradox"

-- | Shared.
pattern ShiftingReality :: EncounterSet
pattern ShiftingReality = Homebrew ":ages-unwound:shifting_reality"

-- | Scenario II.
pattern TheMyriadGentleman :: EncounterSet
pattern TheMyriadGentleman = Homebrew ":ages-unwound:the_myriad_gentleman"

-- | Shared.
pattern Thugs :: EncounterSet
pattern Thugs = Homebrew ":ages-unwound:thugs"

-- | Scenario VII.
pattern TimeRunsOut :: EncounterSet
pattern TimeRunsOut = Homebrew ":ages-unwound:time_runs_out"

-- | Shared.
pattern UnleashedChaos :: EncounterSet
pattern UnleashedChaos = Homebrew ":ages-unwound:unleashed_chaos"

-- | Shared; the guide calls this set "Unravelling Ages".
pattern UnravellingYears :: EncounterSet
pattern UnravellingYears = Homebrew ":ages-unwound:unravelling_years"

-- | Scenario IV.
pattern Unstuck :: EncounterSet
pattern Unstuck = Homebrew ":ages-unwound:unstuck"
