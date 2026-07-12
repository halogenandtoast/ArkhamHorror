{- | Shared plumbing for achievement specs: attach the achievement-eligible
campaign to the standalone test harness (detection and 'earnAchievement'
both gate on the campaign id) and watch for the resulting earn messages.
-}
module Helpers.Achievements (
  module Arkham.Achievement.Types,
  asAchievementCampaign,
  asAchievementCampaignScenario,
  asReturnToNightOfTheZealot,
  asReturnToNightOfTheZealotWith,
  asReturnToNightOfTheZealotScenario,
  asReturnToTheDunwichLegacy,
  asReturnToTheDunwichLegacyWith,
  asReturnToTheDunwichLegacyScenario,
  asReturnToThePathToCarcosa,
  asReturnToThePathToCarcosaWith,
  asReturnToThePathToCarcosaScenario,
  asReturnToTheForgottenAge,
  asReturnToTheForgottenAgeWith,
  asReturnToTheForgottenAgeScenario,
  asReturnToTheCircleUndone,
  asReturnToTheCircleUndoneWith,
  asReturnToTheCircleUndoneScenario,
  didEarn,
  didEarnDunwich,
  didEarnCarcosa,
  didEarnForgottenAge,
  didEarnCircle,
  didProgressDunwich,
  didProgressCarcosa,
  didProgressCircle,
) where

import Arkham.Achievement.Types
import Arkham.Campaign (lookupCampaign)
import Arkham.Campaign.Types (campaignStep)
import Arkham.CampaignStep (CampaignStep (ScenarioStep))
import Arkham.Difficulty
import Arkham.Message
import TestImport

{- | Attach the given campaign at the given difficulty, keeping the harness
scenario.
-}
asAchievementCampaign :: CampaignId -> Difficulty -> TestAppT ()
asAchievementCampaign campaignId difficulty = do
  overTest \g ->
    g
      { gameMode =
          These
            (lookupCampaign campaignId difficulty)
            (fromJustNote "test harness always has a scenario" $ modeScenario (gameMode g))
      }
  tick

{- | Same, but also swap the harness scenario for one with the given id, for
detections scoped to a specific scenario (e.g. The Midnight Masks "50025").
The campaign's step points at that scenario too, so step-sensitive helpers
(e.g. resolution XP reports) see a consistent campaign state.
-}
asAchievementCampaignScenario :: CampaignId -> CardCode -> TestAppT ()
asAchievementCampaignScenario campaignId scenarioCode = do
  scenario' <- testScenario scenarioCode id
  let
    campaign' =
      overAttrs
        (\a -> a {campaignStep = ScenarioStep (ScenarioId scenarioCode)})
        (lookupCampaign campaignId Easy)
  overTest \g -> g {gameMode = These campaign' scenario'}
  tick

asReturnToNightOfTheZealotWith :: Difficulty -> TestAppT ()
asReturnToNightOfTheZealotWith = asAchievementCampaign "50"

asReturnToNightOfTheZealot :: TestAppT ()
asReturnToNightOfTheZealot = asReturnToNightOfTheZealotWith Easy

asReturnToNightOfTheZealotScenario :: CardCode -> TestAppT ()
asReturnToNightOfTheZealotScenario = asAchievementCampaignScenario "50"

asReturnToTheDunwichLegacyWith :: Difficulty -> TestAppT ()
asReturnToTheDunwichLegacyWith = asAchievementCampaign "51"

asReturnToTheDunwichLegacy :: TestAppT ()
asReturnToTheDunwichLegacy = asReturnToTheDunwichLegacyWith Easy

asReturnToTheDunwichLegacyScenario :: CardCode -> TestAppT ()
asReturnToTheDunwichLegacyScenario = asAchievementCampaignScenario "51"

asReturnToThePathToCarcosaWith :: Difficulty -> TestAppT ()
asReturnToThePathToCarcosaWith = asAchievementCampaign "52"

asReturnToThePathToCarcosa :: TestAppT ()
asReturnToThePathToCarcosa = asReturnToThePathToCarcosaWith Easy

asReturnToThePathToCarcosaScenario :: CardCode -> TestAppT ()
asReturnToThePathToCarcosaScenario = asAchievementCampaignScenario "52"

asReturnToTheForgottenAgeWith :: Difficulty -> TestAppT ()
asReturnToTheForgottenAgeWith = asAchievementCampaign "53"

asReturnToTheForgottenAge :: TestAppT ()
asReturnToTheForgottenAge = asReturnToTheForgottenAgeWith Easy

asReturnToTheForgottenAgeScenario :: CardCode -> TestAppT ()
asReturnToTheForgottenAgeScenario = asAchievementCampaignScenario "53"

asReturnToTheCircleUndoneWith :: Difficulty -> TestAppT ()
asReturnToTheCircleUndoneWith = asAchievementCampaign "54"

asReturnToTheCircleUndone :: TestAppT ()
asReturnToTheCircleUndone = asReturnToTheCircleUndoneWith Easy

asReturnToTheCircleUndoneScenario :: CardCode -> TestAppT ()
asReturnToTheCircleUndoneScenario = asAchievementCampaignScenario "54"

didEarn :: NightOfTheZealotAchievement -> TestAppT (IORef Bool)
didEarn achievement =
  createMessageMatcher $ EarnAchievement $ NightOfTheZealotAchievement achievement

didEarnDunwich :: TheDunwichLegacyAchievement -> TestAppT (IORef Bool)
didEarnDunwich achievement =
  createMessageMatcher $ EarnAchievement $ TheDunwichLegacyAchievement achievement

didEarnCarcosa :: ThePathToCarcosaAchievement -> TestAppT (IORef Bool)
didEarnCarcosa achievement =
  createMessageMatcher $ EarnAchievement $ ThePathToCarcosaAchievement achievement

didEarnForgottenAge :: TheForgottenAgeAchievement -> TestAppT (IORef Bool)
didEarnForgottenAge achievement =
  createMessageMatcher $ EarnAchievement $ TheForgottenAgeAchievement achievement

didEarnCircle :: TheCircleUndoneAchievement -> TestAppT (IORef Bool)
didEarnCircle achievement =
  createMessageMatcher $ EarnAchievement $ TheCircleUndoneAchievement achievement

-- Checklist progress reports (cross-playthrough achievements); the items must
-- match exactly, in 'achievementChecklist'-mapping order.

didProgressDunwich :: TheDunwichLegacyAchievement -> [Text] -> TestAppT (IORef Bool)
didProgressDunwich achievement items =
  createMessageMatcher $ AchievementProgress (TheDunwichLegacyAchievement achievement) items

didProgressCarcosa :: ThePathToCarcosaAchievement -> [Text] -> TestAppT (IORef Bool)
didProgressCarcosa achievement items =
  createMessageMatcher $ AchievementProgress (ThePathToCarcosaAchievement achievement) items

didProgressCircle :: TheCircleUndoneAchievement -> [Text] -> TestAppT (IORef Bool)
didProgressCircle achievement items =
  createMessageMatcher $ AchievementProgress (TheCircleUndoneAchievement achievement) items
