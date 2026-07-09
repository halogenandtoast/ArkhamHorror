{- | Shared plumbing for achievement specs: attach the achievement-eligible
campaign to the standalone test harness (detection and 'earnAchievement'
both gate on the campaign id) and watch for the resulting earn messages.
-}
module Helpers.Achievements (
  module Arkham.Achievement.Types,
  asReturnToNightOfTheZealot,
  asReturnToNightOfTheZealotWith,
  asReturnToNightOfTheZealotScenario,
  didEarn,
) where

import Arkham.Achievement.Types
import Arkham.Campaign (lookupCampaign)
import Arkham.Campaign.Types (campaignStep)
import Arkham.CampaignStep (CampaignStep (ScenarioStep))
import Arkham.Difficulty
import Arkham.Id (ScenarioId (..))
import Arkham.Message
import TestImport

{- | Attach the Return to the Night of the Zealot campaign (id "50") at the
given difficulty, keeping the harness scenario.
-}
asReturnToNightOfTheZealotWith :: Difficulty -> TestAppT ()
asReturnToNightOfTheZealotWith difficulty = do
  overTest \g ->
    g
      { gameMode =
          These
            (lookupCampaign "50" difficulty)
            (fromJustNote "test harness always has a scenario" $ modeScenario (gameMode g))
      }
  tick

asReturnToNightOfTheZealot :: TestAppT ()
asReturnToNightOfTheZealot = asReturnToNightOfTheZealotWith Easy

{- | Same, but also swap the harness scenario for one with the given id, for
detections scoped to a specific scenario (e.g. The Midnight Masks "50025").
The campaign's step points at that scenario too, so step-sensitive helpers
(e.g. resolution XP reports) see a consistent campaign state.
-}
asReturnToNightOfTheZealotScenario :: CardCode -> TestAppT ()
asReturnToNightOfTheZealotScenario scenarioCode = do
  scenario' <- testScenario scenarioCode id
  let
    campaign' =
      overAttrs
        (\a -> a {campaignStep = ScenarioStep (ScenarioId scenarioCode)})
        (lookupCampaign "50" Easy)
  overTest \g -> g {gameMode = These campaign' scenario'}
  tick

didEarn :: NightOfTheZealotAchievement -> TestAppT (IORef Bool)
didEarn achievement =
  createMessageMatcher $ EarnAchievement $ NightOfTheZealotAchievement achievement
