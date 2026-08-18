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
  when hasCivilians $ slayCivilianUnchecked lid

{- | Remove a civilian and record it as slain, without first checking that one is
there. Setup needs this because the 'PlaceTokens' messages that put the civilians
out are still queued, so 'slayCivilian''s state read would see none and skip.
-}
slayCivilianUnchecked :: ReverseQueue m => LocationId -> m ()
slayCivilianUnchecked lid = do
  removeTokens ScenarioSource lid Civilian 1
  scenarioCountIncrement CiviliansSlain

becomeAbandonedAbility :: LocationAttrs -> Int -> Ability
becomeAbandonedAbility a n =
  onlyOnce
    $ restricted a n (thisExists a (not_ $ LocationWithToken Civilian) <> NotSetup)
    $ forced AnyWindow
