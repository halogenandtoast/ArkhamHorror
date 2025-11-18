module Arkham.Scenario.SpecHelpers where

import Arkham.Helpers.Scenario
import TestImport

encounterDeckCountIs :: ScenarioId -> Int -> SpecWith ()
encounterDeckCountIs scenarioId expectedCount = it ("has encounter deck count of " <> show expectedCount) . scenarioTest scenarioId $ \_ -> do
  pushAndRun Setup
  Deck deck <- getEncounterDeck
  liftIO $ length deck `shouldBe` expectedCount

encounterDeckCountIsWith :: ScenarioId -> Int -> TestAppT () -> SpecWith ()
encounterDeckCountIsWith scenarioId expectedCount body = it ("has encounter deck count of " <> show expectedCount) . scenarioTest scenarioId $ \_ -> do
  body
  pushAndRun Setup
  Deck deck <- getEncounterDeck
  liftIO $ length deck `shouldBe` expectedCount

