module Arkham.Scenarios.TheHouseAlwaysWins.Helpers where

import Arkham.Campaigns.TheDunwichLegacy.Helpers
import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Name
import Arkham.Prelude
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theHouseAlwaysWins" a

cheated :: HasGame m => m [InvestigatorId]
cheated = do
  allKeys <- setToList <$> scenarioField ScenarioRemembered
  pure $ flip mapMaybe allKeys $ \case
    Cheated (Labeled _ iid) -> Just iid
    _ -> Nothing

hadDrinks :: HasGame m => m [InvestigatorId]
hadDrinks = do
  allKeys <- setToList <$> scenarioField ScenarioRemembered
  pure $ flip mapMaybe allKeys $ \case
    HadADrink x -> Just $ unLabel x
    _ -> Nothing
