module Arkham.Campaigns.TheCircleUndone.Helpers where

import Arkham.Prelude

import Arkham.Ability
import Arkham.CampaignLogKey
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Log
import Arkham.Helpers.Message
import Arkham.Id
import Arkham.Matcher

getHauntedAbilities :: HasGame m => InvestigatorId -> m [Ability]
getHauntedAbilities iid = selectList $ HauntedAbility <> AbilityOnLocation (locationWithInvestigator iid)

runHauntedAbilities :: (HasGame m, HasQueue Message m) => InvestigatorId -> m ()
runHauntedAbilities iid = do
  hauntedAbilities <- getHauntedAbilities iid
  pushWhen (notNull hauntedAbilities)
    $ chooseOneAtATime iid [AbilityLabel iid ab [] [] | ab <- hauntedAbilities]

runLocationHauntedAbilities
  :: (HasGame m, HasQueue Message m) => InvestigatorId -> LocationId -> m ()
runLocationHauntedAbilities iid lid = do
  hauntedAbilities <- selectList $ HauntedAbility <> AbilityOnLocation (LocationWithId lid)
  pushWhen (notNull hauntedAbilities)
    $ chooseOneAtATime iid [AbilityLabel iid ab [] [] | ab <- hauntedAbilities]

getMementosDiscoveredCount :: HasGame m => m Int
getMementosDiscoveredCount = length <$> getRecordSet MementosDiscovered
