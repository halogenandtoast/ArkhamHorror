module Arkham.Message.Lifted.Upgrade where

import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Id
import Arkham.Matcher.Asset
import Arkham.Matcher.Base
import Arkham.Prelude
import Arkham.Query
import Arkham.Target

class Query query => UpgradeTarget query where
  getUpgradeTargets
    :: (HasGame m, QueryElement query ~ a) => InvestigatorId -> query -> m [a]

instance UpgradeTarget AssetMatcher where
  getUpgradeTargets iid q = do
    mods <- getModifiers iid
    let ifAble = nub [aid | UpgradeTargetIfAble (AssetTarget aid) <- mods]
    if null ifAble
      then select q
      else do
        xs <- select $ q <> mapOneOf AssetWithId ifAble
        if null xs then select q else pure xs
