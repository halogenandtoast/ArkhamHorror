module Arkham.Helpers.Ability where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes.HasAbilities
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Projection

getIsUnused' :: (Monad m, HasGame m) => InvestigatorId -> Ability -> m Bool
getIsUnused' iid ability = do
  usedAbilities <- fieldMap InvestigatorUsedAbilities (map usedAbility) iid
  pure $ ability `notElem` usedAbilities

getGroupIsUnused :: (Monad m, HasGame m) => Ability -> m Bool
getGroupIsUnused ability = do
  investigatorIds <- getInvestigatorIds
  usedAbilities <- concatMapM
    (fieldMap InvestigatorUsedAbilities (map usedAbility))
    investigatorIds
  pure $ ability `notElem` usedAbilities

withBaseAbilities :: HasAbilities a => a -> [Ability] -> [Ability]
withBaseAbilities a f = getAbilities a <> f

