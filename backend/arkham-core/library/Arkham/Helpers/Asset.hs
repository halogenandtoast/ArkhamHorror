module Arkham.Helpers.Asset where

import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Source

assetCanHaveHorrorHealed :: (HasGame m, Sourceable a) => a -> AssetId -> m Bool
assetCanHaveHorrorHealed a = selectAny . HealableAsset (toSource a) #horror . AssetWithId

assetCanHaveDamageHealed :: (HasGame m, Sourceable a) => a -> AssetId -> m Bool
assetCanHaveDamageHealed a = selectAny . HealableAsset (toSource a) #damage . AssetWithId
