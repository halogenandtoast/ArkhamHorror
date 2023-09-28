module Arkham.Campaigns.TheCircleUndone.Helpers where

import Arkham.Prelude

import Arkham.Ability
import Arkham.CampaignLogKey
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Log
import Arkham.Helpers.Message
import Arkham.Id
import Arkham.Matcher
import Arkham.Message

getHauntedAbilities :: HasGame m => InvestigatorId -> m [Ability]
getHauntedAbilities iid =
  selectList
    $ HauntedAbility
    <> AbilityOnLocation
      (locationWithInvestigator iid)

runHauntedAbilities :: InvestigatorId -> GameT ()
runHauntedAbilities iid = do
  hauntedAbilities <- getHauntedAbilities iid
  pushWhen (notNull hauntedAbilities)
    $ chooseOneAtATime
      iid
      [AbilityLabel iid ab [] [] | ab <- hauntedAbilities]

runLocationHauntedAbilities :: InvestigatorId -> LocationId -> GameT ()
runLocationHauntedAbilities iid lid = do
  hauntedAbilities <- selectList $ HauntedAbility <> AbilityOnLocation (LocationWithId lid)
  pushWhen (notNull hauntedAbilities)
    $ chooseOneAtATime
      iid
      [AbilityLabel iid ab [] [] | ab <- hauntedAbilities]

getMementosDiscoveredCount :: HasGame m => m Int
getMementosDiscoveredCount = length <$> getRecordSet MementosDiscovered
