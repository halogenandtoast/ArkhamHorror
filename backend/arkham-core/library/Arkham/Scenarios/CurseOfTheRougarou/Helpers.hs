module Arkham.Scenarios.CurseOfTheRougarou.Helpers where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Json
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Trait

bayouLocations :: HasGame m => m (Set LocationId)
bayouLocations = select $ LocationWithTrait Bayou

nonBayouLocations :: HasGame m => m (Set LocationId)
nonBayouLocations = select $ LocationWithoutTrait Bayou

getTheRougarou :: HasGame m => m (Maybe EnemyId)
getTheRougarou = selectOne $ enemyIs Cards.theRougarou

locationsWithLabels :: MonadRandom m => Trait -> [Card] -> m [(Text, Card)]
locationsWithLabels trait locationSet = do
  let
    (before, bayou :| after) = case break (elem Bayou . toTraits) locationSet of
      (_, []) -> error "not expected"
      (a, b : c) -> (a, b :| c)
  shuffled <- shuffleM (before <> after)
  pure $ zip labels (bayou : shuffled)
 where
  labels =
    [ pack (camelCase $ show trait) <> "Bayou"
    , pack (camelCase $ show trait) <> "1"
    , pack (camelCase $ show trait) <> "2"
    ]

locationsByTrait :: Map Trait [CardDef]
locationsByTrait =
  mapFromList
    [
      ( NewOrleans
      , [Locations.cursedShores, Locations.gardenDistrict, Locations.broadmoor]
      )
    ,
      ( Riverside
      ,
        [ Locations.brackishWaters
        , Locations.audubonPark
        , Locations.faubourgMarigny
        ]
      )
    ,
      ( Wilderness
      ,
        [ Locations.forgottenMarsh
        , Locations.trappersCabin
        , Locations.twistedUnderbrush
        ]
      )
    ,
      ( Unhallowed
      , [Locations.foulSwamp, Locations.ritualGrounds, Locations.overgrownCairns]
      )
    ]
