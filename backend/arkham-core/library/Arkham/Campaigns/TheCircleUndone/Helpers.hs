module Arkham.Campaigns.TheCircleUndone.Helpers where

import Arkham.Prelude

import Arkham.Classes.Query
import Arkham.Ability
import {-# SOURCE #-} Arkham.GameEnv
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Matcher

getHauntedAbilities :: HasGame m => InvestigatorId -> m [Ability]
getHauntedAbilities iid = selectList $ HauntedAbility <> AbilityOnLocation
  (locationWithInvestigator iid)

