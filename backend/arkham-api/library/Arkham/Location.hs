{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Location (
  module Arkham.Location,
  module X,
) where

import Arkham.Card
import Arkham.Classes
import Arkham.Custom.Location (customLocation)
import Arkham.Helpers.Modifiers
import Arkham.Homebrew.Registry qualified as Registry
import Arkham.Id
import Arkham.Location.Locations
import Arkham.Location.Runner
import Arkham.Location.Types as X (Location)
import Arkham.Prelude

createLocation :: IsCard a => a -> LocationId -> Location
createLocation a lid = lookupLocation (toCardCode a) lid (toCardId a)

lookupLocation :: HasCallStack => CardCode -> LocationId -> CardId -> Location
lookupLocation cCode = case lookup cCode allLocations of
  Just (SomeLocationCard a) -> \lid cid -> Location $ cbCardBuilder a cid lid
  Nothing -> case lookupCustomCardDefOrMissing LocationType cCode of
    Just def -> \lid cid -> Location $ cbCardBuilder (customLocation def) cid lid
    Nothing -> error $ "Unknown location: " <> show cCode <> "\n" <> prettyCallStack callStack

instance RunMessage Location where
  runMessage (Reset target) x | isTarget (toAttrs x) target = do
    let a = toAttrs x
    pure
      $ overAttrs
        (\y -> y {locationLabel = locationLabel a, locationDirections = locationDirections a})
        (lookupLocation (toCardCode a) a.id (toCardId a))
  runMessage msg x@(Location l) = do
    modifiers' <- getModifiers (toTarget x)
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    Location <$> runMessage msg' l

instance FromJSON Location where
  parseJSON = withObject "Location" $ \o -> do
    cCode <- o .: "cardCode"
    withLocationCardCode cCode
      $ \(_ :: LocationCard a) -> Location <$> parseJSON @a (Object o)

withLocationCardCode
  :: CardCode -> (forall a. IsLocation a => LocationCard a -> r) -> r
withLocationCardCode cCode f = case lookup cCode allLocations of
  Just (SomeLocationCard a) -> f a
  Nothing -> case lookupCustomCardDefOrMissing LocationType cCode of
    Just def -> f (customLocation def)
    Nothing -> error "invalid locations"

allLocations :: Map CardCode SomeLocationCard
allLocations =
  (mapFrom someLocationCardCode Registry.locations <>)
    $ mapFrom someLocationCardCode allLocationCardBuilders
