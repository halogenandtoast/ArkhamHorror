{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Location
  ( module Arkham.Location
  , module X
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Card.Id
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Location.Types as X ( Location )
import Arkham.Location.Locations
import Arkham.Location.Runner
import Arkham.Message
import Data.UUID ( nil )

createLocation :: IsCard a => a -> Location
createLocation a = lookupLocation (toCardCode a) (LocationId $ toCardId a)

lookupLocationStub :: CardCode -> Location
lookupLocationStub = ($ LocationId (CardId nil)) . lookupLocation

lookupLocation :: CardCode -> (LocationId -> Location)
lookupLocation lid = case lookup lid allLocations of
  Nothing -> error $ "Unknown location: " <> show lid
  Just (SomeLocationCard a) -> Location <$> cbCardBuilder a

instance RunMessage Location where
  runMessage msg x@(Location l) = do
    modifiers' <- getModifiers (toTarget x)
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    Location <$> runMessage msg' l

instance FromJSON Location where
  parseJSON v = flip (withObject "Location") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withLocationCardCode cCode
      $ \(_ :: LocationCard a) -> Location <$> parseJSON @a v

withLocationCardCode
  :: CardCode -> (forall a . IsLocation a => LocationCard a -> r) -> r
withLocationCardCode cCode f = case lookup cCode allLocations of
  Nothing -> error "invalid locations"
  Just (SomeLocationCard a) -> f a

allLocations :: HashMap CardCode SomeLocationCard
allLocations = mapFromList $ map
  (toFst someLocationCardCode)
  [ -- Night of the Zealot
    -- The Gathering
    SomeLocationCard study
  , SomeLocationCard hallway
  , SomeLocationCard attic
  , SomeLocationCard cellar
  , SomeLocationCard parlor
  ]

