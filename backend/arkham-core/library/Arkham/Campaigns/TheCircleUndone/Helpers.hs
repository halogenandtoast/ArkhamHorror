module Arkham.Campaigns.TheCircleUndone.Helpers where

import Arkham.Prelude

import Arkham.Ability
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Log
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Store

getHauntedAbilities :: (HasGame m, Store m Card) => InvestigatorId -> m [Ability]
getHauntedAbilities iid =
  selectList $
    HauntedAbility
      <> AbilityOnLocation
        (locationWithInvestigator iid)

runHauntedAbilities :: (HasGame m, Store m Card, HasQueue Message m) => InvestigatorId -> m ()
runHauntedAbilities iid = do
  hauntedAbilities <- getHauntedAbilities iid
  when (notNull hauntedAbilities) $
    push $
      chooseOneAtATime
        iid
        [AbilityLabel iid ab [] [] | ab <- hauntedAbilities]

getMementosDiscoveredCount :: (HasGame m, Store m Card) => m Int
getMementosDiscoveredCount = length <$> getRecordSet MementosDiscovered
