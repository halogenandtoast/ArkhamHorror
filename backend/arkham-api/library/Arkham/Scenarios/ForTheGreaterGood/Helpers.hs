module Arkham.Scenarios.ForTheGreaterGood.Helpers where

import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.ChaosToken.Types
import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Key
import Arkham.Prelude
import Arkham.Scenario.Types

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "forTheGreaterGood" a

getRandomKey :: (HasGame m, MonadRandom m) => m (Maybe ArkhamKey)
getRandomKey = do
  ks <- setToList <$> scenarioField ScenarioSetAsideKeys
  isReturnTo <- getIsReturnTo
  if isReturnTo && null ks
    then
      fmap (Just . TokenKey)
        . createChaosToken
        =<< sample
          ( PlusOne
              :| [MinusOne, MinusTwo, MinusThree, MinusFour, MinusFive, MinusSix, MinusSeven, MinusEight]
          )
    else traverse sample $ nonEmpty ks
