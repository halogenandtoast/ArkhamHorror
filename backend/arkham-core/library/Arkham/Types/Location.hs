{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location
  ( lookupLocation
  , isBlocked
  , Location(..)
  )
where

import Arkham.Json
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Cards.Attic
import Arkham.Types.Location.Cards.Cellar
import Arkham.Types.Location.Cards.Hallway
import Arkham.Types.Location.Cards.Parlor
import Arkham.Types.Location.Cards.Study
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Safe (fromJustNote)

lookupLocation :: LocationId -> Location
lookupLocation lid =
  fromJustNote ("Unkown location: " <> show lid)
    $ HashMap.lookup lid allLocations

isBlocked :: Location -> Bool
isBlocked = locationBlocked . locationAttrs

allLocations :: HashMap LocationId Location
allLocations = HashMap.fromList $ map
  (\s -> (locationId . locationAttrs $ s, s))
  [Study' study, Hallway' hallway, Attic' attic, Cellar' cellar, Parlor' parlor]

instance HasAbilities Location where
  getAbilities = locationAbilities . locationAttrs

instance HasCount ClueCount () Location where
  getCount _ = ClueCount . locationClues . locationAttrs

instance HasSet EnemyId () Location where
  getSet _ = locationEnemies . locationAttrs

instance HasSet TreacheryId () Location where
  getSet _ = locationTreacheries . locationAttrs

instance HasSet AssetId () Location where
  getSet _ = locationAssets . locationAttrs

instance HasSet InvestigatorId () Location where
  getSet _ = locationInvestigators . locationAttrs

instance HasSet ConnectedLocationId () Location where
  getSet _ =
    HashSet.map ConnectedLocationId . locationConnectedLocations . locationAttrs

data Location
  = Study' Study
  | Hallway' Hallway
  | Attic' Attic
  | Cellar' Cellar
  | Parlor' Parlor
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

locationAttrs :: Location -> Attrs
locationAttrs = \case
  Study' attrs -> coerce attrs
  Hallway' attrs -> coerce attrs
  Attic' attrs -> coerce attrs
  Cellar' attrs -> coerce attrs
  Parlor' attrs -> coerce attrs

instance (LocationRunner env) => RunMessage env Location where
  runMessage msg = \case
    Study' x -> Study' <$> runMessage msg x
    Hallway' x -> Hallway' <$> runMessage msg x
    Attic' x -> Attic' <$> runMessage msg x
    Cellar' x -> Cellar' <$> runMessage msg x
    Parlor' x -> Parlor' <$> runMessage msg x
