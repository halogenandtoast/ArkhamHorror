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
  asTheDrownedCity,
  asTheDrownedCityWith,
  asTheDrownedCityScenario,
  asTheDreamEaters,
  asTheDreamEatersWith,
  asTheDreamEatersScenario,
  asTheInnsmouthConspiracy,
  asTheInnsmouthConspiracyWith,
  asTheInnsmouthConspiracyScenario,
  asEdgeOfTheEarth,
  asEdgeOfTheEarthWith,
  asEdgeOfTheEarthScenario,
  asTheScarletKeys,
  asTheScarletKeysWith,
  asTheScarletKeysScenario,
  asTheFeastOfHemlockVale,
  asTheFeastOfHemlockValeWith,
  asTheFeastOfHemlockValeScenario,
  didEarn,
  didEarnDunwich,
  didEarnCarcosa,
  didEarnForgottenAge,
  didEarnCircle,
  didEarnDrownedCity,
  didEarnDreamQuest,
  didEarnWebOfDreams,
  didEarnInnsmouth,
  didEarnEdgeOfTheEarth,
  didEarnScarletKeys,
  didEarnHemlockVale,
  didProgressDunwich,
  didProgressCarcosa,
  didProgressCircle,
  didProgressDrownedCity,
  didProgressInnsmouth,
  didProgressEdgeOfTheEarth,
  didProgressScarletKeys,
  didProgressHemlockVale,
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

-- The Drowned City's list is printed for the campaign itself, so campaign "11"
-- is the eligible campaign rather than a Return-to variant.

asTheDrownedCityWith :: Difficulty -> TestAppT ()
asTheDrownedCityWith = asAchievementCampaign "11"

asTheDrownedCity :: TestAppT ()
asTheDrownedCity = asTheDrownedCityWith Easy

asTheDrownedCityScenario :: CardCode -> TestAppT ()
asTheDrownedCityScenario = asAchievementCampaignScenario "11"

-- The Dream-Eaters prints one achievement list per mini-campaign, but both are
-- earned in campaign "06"; detection is scoped by scenario instead.

asTheDreamEatersWith :: Difficulty -> TestAppT ()
asTheDreamEatersWith = asAchievementCampaign "06"

asTheDreamEaters :: TestAppT ()
asTheDreamEaters = asTheDreamEatersWith Easy

asTheDreamEatersScenario :: CardCode -> TestAppT ()
asTheDreamEatersScenario = asAchievementCampaignScenario "06"

-- The Innsmouth Conspiracy's list is printed for the campaign itself, so
-- campaign "07" is the eligible campaign rather than a Return-to variant.

asTheInnsmouthConspiracyWith :: Difficulty -> TestAppT ()
asTheInnsmouthConspiracyWith = asAchievementCampaign "07"

asTheInnsmouthConspiracy :: TestAppT ()
asTheInnsmouthConspiracy = asTheInnsmouthConspiracyWith Easy

asTheInnsmouthConspiracyScenario :: CardCode -> TestAppT ()
asTheInnsmouthConspiracyScenario = asAchievementCampaignScenario "07"

-- Edge of the Earth's list is printed for the campaign itself, so campaign "08"
-- is the eligible campaign rather than a Return-to variant.

asEdgeOfTheEarthWith :: Difficulty -> TestAppT ()
asEdgeOfTheEarthWith = asAchievementCampaign "08"

asEdgeOfTheEarth :: TestAppT ()
asEdgeOfTheEarth = asEdgeOfTheEarthWith Easy

asEdgeOfTheEarthScenario :: CardCode -> TestAppT ()
asEdgeOfTheEarthScenario = asAchievementCampaignScenario "08"

-- The Scarlet Keys' list is printed for the campaign itself, so campaign "09"
-- is the eligible campaign rather than a Return-to variant.

asTheScarletKeysWith :: Difficulty -> TestAppT ()
asTheScarletKeysWith = asAchievementCampaign "09"

asTheScarletKeys :: TestAppT ()
asTheScarletKeys = asTheScarletKeysWith Easy

asTheScarletKeysScenario :: CardCode -> TestAppT ()
asTheScarletKeysScenario = asAchievementCampaignScenario "09"

-- The Feast of Hemlock Vale's list is printed for the campaign itself, so campaign
-- "10" is the eligible campaign rather than a Return-to variant.

asTheFeastOfHemlockValeWith :: Difficulty -> TestAppT ()
asTheFeastOfHemlockValeWith = asAchievementCampaign "10"

asTheFeastOfHemlockVale :: TestAppT ()
asTheFeastOfHemlockVale = asTheFeastOfHemlockValeWith Easy

asTheFeastOfHemlockValeScenario :: CardCode -> TestAppT ()
asTheFeastOfHemlockValeScenario = asAchievementCampaignScenario "10"

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

didEarnDrownedCity :: TheDrownedCityAchievement -> TestAppT (IORef Bool)
didEarnDrownedCity achievement =
  createMessageMatcher $ EarnAchievement $ TheDrownedCityAchievement achievement

didEarnDreamQuest :: TheDreamQuestAchievement -> TestAppT (IORef Bool)
didEarnDreamQuest achievement =
  createMessageMatcher $ EarnAchievement $ TheDreamQuestAchievement achievement

didEarnWebOfDreams :: TheWebOfDreamsAchievement -> TestAppT (IORef Bool)
didEarnWebOfDreams achievement =
  createMessageMatcher $ EarnAchievement $ TheWebOfDreamsAchievement achievement

didEarnInnsmouth :: TheInnsmouthConspiracyAchievement -> TestAppT (IORef Bool)
didEarnInnsmouth achievement =
  createMessageMatcher $ EarnAchievement $ TheInnsmouthConspiracyAchievement achievement

didEarnEdgeOfTheEarth :: EdgeOfTheEarthAchievement -> TestAppT (IORef Bool)
didEarnEdgeOfTheEarth achievement =
  createMessageMatcher $ EarnAchievement $ EdgeOfTheEarthAchievement achievement

didEarnScarletKeys :: TheScarletKeysAchievement -> TestAppT (IORef Bool)
didEarnScarletKeys achievement =
  createMessageMatcher $ EarnAchievement $ TheScarletKeysAchievement achievement

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

didProgressDrownedCity :: TheDrownedCityAchievement -> [Text] -> TestAppT (IORef Bool)
didProgressDrownedCity achievement items =
  createMessageMatcher $ AchievementProgress (TheDrownedCityAchievement achievement) items

didProgressInnsmouth :: TheInnsmouthConspiracyAchievement -> [Text] -> TestAppT (IORef Bool)
didProgressInnsmouth achievement items =
  createMessageMatcher $ AchievementProgress (TheInnsmouthConspiracyAchievement achievement) items

didProgressEdgeOfTheEarth :: EdgeOfTheEarthAchievement -> [Text] -> TestAppT (IORef Bool)
didProgressEdgeOfTheEarth achievement items =
  createMessageMatcher $ AchievementProgress (EdgeOfTheEarthAchievement achievement) items

didEarnHemlockVale :: TheFeastOfHemlockValeAchievement -> TestAppT (IORef Bool)
didEarnHemlockVale achievement =
  createMessageMatcher $ EarnAchievement $ TheFeastOfHemlockValeAchievement achievement

didProgressHemlockVale :: TheFeastOfHemlockValeAchievement -> [Text] -> TestAppT (IORef Bool)
didProgressHemlockVale achievement items =
  createMessageMatcher $ AchievementProgress (TheFeastOfHemlockValeAchievement achievement) items

didProgressScarletKeys :: TheScarletKeysAchievement -> [Text] -> TestAppT (IORef Bool)
didProgressScarletKeys achievement items =
  createMessageMatcher $ AchievementProgress (TheScarletKeysAchievement achievement) items
