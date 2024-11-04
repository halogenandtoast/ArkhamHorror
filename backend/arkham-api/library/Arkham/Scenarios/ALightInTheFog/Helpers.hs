module Arkham.Scenarios.ALightInTheFog.Helpers where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Helpers.Query (getLead)
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message (Message (ForInvestigator, ScenarioSpecific))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "aLightInTheFog" a

captured :: ReverseQueue m => InvestigatorId -> m ()
captured iid = push $ ForInvestigator iid $ ScenarioSpecific "captured" Null

floodBottommost_ :: ReverseQueue m => Int -> m ()
floodBottommost_ = void . floodBottommost

floodBottommost :: ReverseQueue m => Int -> m Bool
floodBottommost n' = do
  x <- selectCount CanHaveFloodLevelIncreased
  if x < n'
    then pure False
    else do
      go (-3) n'
      pure True
 where
  go _ 0 = pure ()
  go row n = do
    ls <- select $ LocationInRow row <> CanHaveFloodLevelIncreased
    if length ls > n
      then do
        lead <- getLead
        chooseNM lead n $ targets ls increaseThisFloodLevel
      else do
        for_ ls increaseThisFloodLevel
        go (row + 1) (n - length ls)
