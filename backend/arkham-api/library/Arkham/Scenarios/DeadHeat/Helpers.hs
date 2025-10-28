module Arkham.Scenarios.DeadHeat.Helpers where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Helpers.Log (scenarioCountIncrement)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Types (Field (..), LocationAttrs)
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Source
import Arkham.Token

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "deadHeat" a

slayCivilian :: ReverseQueue m => LocationId -> m ()
slayCivilian lid = do
  hasCivilians <- fieldMap LocationTokens (hasToken Civilian) lid
  when hasCivilians $ do
    removeTokens ScenarioSource lid Civilian 1
    scenarioCountIncrement CiviliansSlain

becomeAbandonedAbility :: LocationAttrs -> Int -> Ability
becomeAbandonedAbility a n =
  onlyOnce
    $ restricted a n (thisExists a (not_ $ LocationWithToken Civilian) <> NotSetup)
    $ forced AnyWindow
