-- Scenario shell only: setup, token effects and resolutions are not implemented.
module Arkham.Homebrew.NightOfVespers.Scenarios.TheTeachingsOfAradia (theTeachingsOfAradia) where

import Arkham.Helpers.FlavorText
import Arkham.Homebrew.NightOfVespers.Helpers
import Arkham.Homebrew.NightOfVespers.Intros
import Arkham.Homebrew.NightOfVespers.Jinx (runJinx)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Import.Lifted
import Arkham.Strategy
import Arkham.Trait qualified as Trait
import Arkham.Zone (Zone (FromDeck))

newtype TheTeachingsOfAradia = TheTeachingsOfAradia ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

theTeachingsOfAradia :: Difficulty -> TheTeachingsOfAradia
theTeachingsOfAradia difficulty = scenario TheTeachingsOfAradia ":night-of-vespers:105" "The Teachings of Aradia" difficulty []

instance RunMessage TheTeachingsOfAradia where
  runMessage msg s@(TheTeachingsOfAradia attrs) =
    runQueueT $ scenarioI18n "theTeachingsOfAradia" $ runJinx msg *> case msg of
      PreScenarioSetup -> do
        readIntro 1
        readers <- select $ mapOneOf InvestigatorWithTrait [Trait.Dreamer, Trait.Sorcerer, Trait.Witch]
        unless (null readers) $ leadChooseOneM do
          questionLabeled "reader"
          targets readers \iid -> do
            storyOnlyBuild [iid] $ h "intro.title" >> p "intro.dejaVu" >> p.basic "intro.bonus"
            -- The guide says put into play, not play: defer the search so the
            -- selected card bypasses playing restrictions and additional costs.
            search
              iid
              ScenarioSource
              iid
              [(FromDeck, ShuffleBackIn)]
              (basic $ #asset <> CardFillsSlot #arcane)
              (defer ScenarioTarget IsNotDraw)
        introStep 2
        pure s
      DoStep _ PreScenarioSetup -> do
        readIntro 2
        pure s
      SearchFound iid ScenarioTarget _ cards -> do
        unless (null cards) $ focusCards cards do
          chooseOneM iid do
            targets cards $ putCardIntoPlay iid
            labeled "declineAsset" nothing
        pure s
      Setup -> do
        flavor $ scope "rules" $ h "title" >> p "body"
        flavor $ scope "scaffold" $ h "title" >> p "body"
        pure s
      _ -> TheTeachingsOfAradia <$> liftRunMessage msg attrs
