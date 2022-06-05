module Arkham.Helpers.Ability where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes.Query
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Investigator.Attrs ( Field (..), InvestigatorAttrs )
import Arkham.Matcher
import Arkham.Projection

getIsUnused'
  :: (Monad m, Projection m InvestigatorAttrs)
  => InvestigatorId
  -> Ability
  -> m Bool
getIsUnused' iid ability = do
  usedAbilities <- fieldMap InvestigatorUsedAbilities (map usedAbility) iid
  pure $ ability `notElem` usedAbilities

getGroupIsUnused
  :: (Monad m, Projection m InvestigatorAttrs, Query InvestigatorMatcher m)
  => Ability
  -> m Bool
getGroupIsUnused ability = do
  investigatorIds <- getInvestigatorIds
  usedAbilities <- concatMapM
    (fieldMap InvestigatorUsedAbilities (map usedAbility))
    investigatorIds
  pure $ ability `notElem` usedAbilities
