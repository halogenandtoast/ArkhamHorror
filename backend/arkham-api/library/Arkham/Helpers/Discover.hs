module Arkham.Helpers.Discover where

import Arkham.Classes.HasGame
import Arkham.Discover
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Tracing

getDiscoverLocation :: (HasGame m, Tracing m) => InvestigatorId -> Discover -> m (Maybe LocationId)
getDiscoverLocation iid d = case d.location of
  DiscoverAtLocation lid' -> pure (Just lid')
  DiscoverYourLocation -> field InvestigatorLocation iid

getDiscoveredTotal :: (HasGame m, Tracing m) => InvestigatorId -> Discover -> m Int
getDiscoveredTotal iid d = getDiscoverLocation iid d >>= \case
  Nothing -> pure 0
  Just lid -> do
    mods <- getModifiers iid
    let additionalDiscovered = getSum $ fold [Sum x | d.isInvestigate == IsInvestigate, DiscoveredClues x <- mods]
    base <- total lid (d.count + additionalDiscovered)
    min base <$> field LocationClues lid
 where
  total lid' n = do
    let
      getMaybeMax :: ModifierType -> Maybe Int -> Maybe Int
      getMaybeMax (MaxCluesDiscovered x) Nothing = Just x
      getMaybeMax (MaxCluesDiscovered x) (Just x') = Just $ min x x'
      getMaybeMax _ x = x
    mMax :: Maybe Int <- foldr getMaybeMax Nothing <$> getModifiers lid'
    pure $ maybe n (min n) mMax
