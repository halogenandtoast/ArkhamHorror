-- Scenario shell only: setup, token effects and resolutions are not implemented.
module Arkham.Homebrew.NightOfVespers.Scenarios.TheCityOfFog (theCityOfFog) where

import Arkham.Helpers.FlavorText
import Arkham.Homebrew.NightOfVespers.Helpers
import Arkham.Homebrew.NightOfVespers.Intros
import Arkham.Homebrew.NightOfVespers.Jinx (runJinx)
import Arkham.Homebrew.NightOfVespers.Key
import Arkham.Scenario.Import.Lifted

newtype TheCityOfFog = TheCityOfFog ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

theCityOfFog :: Difficulty -> TheCityOfFog
theCityOfFog difficulty = scenario TheCityOfFog ":night-of-vespers:136" "The City of Fog" difficulty []

instance RunMessage TheCityOfFog where
  runMessage msg s@(TheCityOfFog attrs) =
    runQueueT $ scenarioI18n "theCityOfFog" $ runJinx msg *> case msg of
      PreScenarioSetup -> do
        readIntro 1
        chooseLead
          [ (2, YouLearnedAboutTheTenebrarumMurders)
          , (3, YouWereInvitedToJoinTheTenebrarumLodge)
          , (4, DrArmitageTaughtYouAboutTheThreeMothers)
          , (5, AnnaKaslowDirectedYouTowardAnOldColleague)
          , (6, MaddalenaGuidesYourPath)
          ]
        pure s
      DoStep 7 PreScenarioSetup -> do
        readIntro 7 >> pEffect "companion"
        pure s
      DoStep n PreScenarioSetup -> do
        readIntro n
        when (n `elem` [2, 3]) $ pEffect ("lead" <> tshow n)
        whenM (getHasRecord FinlayODoyleIsAssistingTheInvestigation) $ introStep 7
        pure s
      Setup -> do
        flavor $ scope "rules" $ h "title" >> p "body"
        flavor $ scope "scaffold" $ h "title" >> p "body"
        pure s
      _ -> TheCityOfFog <$> liftRunMessage msg attrs
