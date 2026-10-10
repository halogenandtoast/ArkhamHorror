{- | The Ages Unwound step graph: seven scenarios, a prologue, one interlude and
an epilogue. A scenario step's id is its scenario reference card's code.
-}
module Arkham.Homebrew.AgesUnwound.CampaignSteps where

import Arkham.CampaignStep
import Arkham.Prelude

-- Scenario I
pattern NightOfFire :: CampaignStep
pattern NightOfFire <- ScenarioStep ":ages-unwound:001"
  where
    NightOfFire = ScenarioStep ":ages-unwound:001"

-- Scenario II
pattern TheMyriadGentleman :: CampaignStep
pattern TheMyriadGentleman <- ScenarioStep ":ages-unwound:023"
  where
    TheMyriadGentleman = ScenarioStep ":ages-unwound:023"

-- Scenario III
pattern AWorldTornDown :: CampaignStep
pattern AWorldTornDown <- ScenarioStep ":ages-unwound:048"
  where
    AWorldTornDown = ScenarioStep ":ages-unwound:048"

-- Scenario IV
pattern Unstuck :: CampaignStep
pattern Unstuck <- ScenarioStep ":ages-unwound:062"
  where
    Unstuck = ScenarioStep ":ages-unwound:062"

-- Scenario V
pattern AYearToPlan :: CampaignStep
pattern AYearToPlan <- ScenarioStep ":ages-unwound:105"
  where
    AYearToPlan = ScenarioStep ":ages-unwound:105"

-- Scenario VI
pattern AWorldTornDownAgain :: CampaignStep
pattern AWorldTornDownAgain <- ScenarioStep ":ages-unwound:155"
  where
    AWorldTornDownAgain = ScenarioStep ":ages-unwound:155"

-- Scenario VII
pattern TimeRunsOut :: CampaignStep
pattern TimeRunsOut <- ScenarioStep ":ages-unwound:182"
  where
    TimeRunsOut = ScenarioStep ":ages-unwound:182"

-- | Interlude I: An Unknown Benefactor. Choice of Going It Alone / Leap of Faith.
pattern AnUnknownBenefactor :: CampaignStep
pattern AnUnknownBenefactor = InterludeStep 1 Nothing
