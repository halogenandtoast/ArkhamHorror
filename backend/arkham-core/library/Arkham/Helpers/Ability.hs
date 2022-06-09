module Arkham.Helpers.Ability where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Helpers.Query
import Arkham.Id
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Projection

getIsUnused'
  :: InvestigatorId
  -> Ability
  -> GameT Bool
getIsUnused' iid ability = do
  usedAbilities <- fieldF InvestigatorUsedAbilities (map usedAbility) iid
  pure $ ability `notElem` usedAbilities

getGroupIsUnused
  :: Ability
  -> GameT Bool
getGroupIsUnused ability = do
  investigatorIds <- getInvestigatorIds
  usedAbilities <- concatMapM
    (fieldF InvestigatorUsedAbilities (map usedAbility))
    investigatorIds
  pure $ ability `notElem` usedAbilities
