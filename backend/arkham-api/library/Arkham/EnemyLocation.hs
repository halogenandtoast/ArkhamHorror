{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.EnemyLocation (
  module Arkham.EnemyLocation,
  module X,
) where

import Arkham.Card
import Arkham.EnemyLocation.Runner
import Arkham.EnemyLocation.Types as X (EnemyLocation (..))
import Arkham.Id
import Arkham.Prelude

-- | Create an enemy-location entity from a card and location id.
-- The card's code must exist in 'allEnemyLocations'.
createEnemyLocation :: IsCard a => a -> LocationId -> EnemyLocation
createEnemyLocation a lid = lookupEnemyLocation (toCardCode a) lid (toCardId a)

-- | Look up an enemy-location card by its code and instantiate it.
lookupEnemyLocation :: HasCallStack => CardCode -> LocationId -> CardId -> EnemyLocation
lookupEnemyLocation cCode = case lookup cCode allEnemyLocations of
  Nothing -> error $ "Unknown enemy-location: " <> show cCode
  Just (SomeEnemyLocationCard a) -> \lid cid -> EnemyLocation $ cbCardBuilder a cid lid

instance RunMessage EnemyLocation where
  runMessage msg (EnemyLocation x) =
    EnemyLocation <$> runMessage msg x

instance FromJSON EnemyLocation where
  parseJSON = withObject "EnemyLocation" $ \o -> do
    cCode <- o .: "cardCode"
    withEnemyLocationCardCode cCode
      $ \(_ :: EnemyLocationCard a) -> EnemyLocation <$> parseJSON @a (Object o)

withEnemyLocationCardCode
  :: CardCode -> (forall a. IsEnemyLocation a => EnemyLocationCard a -> r) -> r
withEnemyLocationCardCode cCode f = case lookup cCode allEnemyLocations of
  Nothing -> error $ "Unknown enemy-location: " <> show cCode
  Just (SomeEnemyLocationCard a) -> f a

-- | All registered enemy-location cards across all scenarios.
-- New enemy-location cards are registered here as they are implemented.
allEnemyLocations :: Map CardCode SomeEnemyLocationCard
allEnemyLocations = mapFrom someEnemyLocationCardCode []
