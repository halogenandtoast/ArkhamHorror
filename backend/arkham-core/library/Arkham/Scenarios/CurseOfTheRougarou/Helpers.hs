module Arkham.Scenarios.CurseOfTheRougarou.Helpers where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Json
import Arkham.Location.Cards qualified as Locations
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Trait

bayouLocations
  :: (MonadReader env m, HasSet LocationId env [Trait])
  => m (HashSet LocationId)
bayouLocations = getSet [Bayou]

nonBayouLocations
  :: ( MonadReader env m
     , HasSet LocationId env ()
     , HasSet LocationId env [Trait]
     )
  => m (HashSet LocationId)
nonBayouLocations = difference <$> getLocationSet <*> bayouLocations

getTheRougarou
  :: (MonadReader env m, Query EnemyMatcher env) => m (Maybe EnemyId)
getTheRougarou = selectOne $ enemyIs Cards.theRougarou

locationsWithLabels :: MonadRandom m => Trait -> [Card] -> m [(Text, Card)]
locationsWithLabels trait locationSet = do
  shuffled <- shuffleM (before <> after)
  pure $ zip labels (bayou : shuffled)
 where
  labels =
    [ pack (camelCase $ show trait) <> "Bayou"
    , pack (camelCase $ show trait) <> "1"
    , pack (camelCase $ show trait) <> "2"
    ]
  (before, bayou : after) = break (elem Bayou . toTraits) locationSet

locationsByTrait :: HashMap Trait [CardDef]
locationsByTrait = mapFromList
  [ ( NewOrleans
    , [Locations.cursedShores, Locations.gardenDistrict, Locations.broadmoor]
    )
  , ( Riverside
    , [ Locations.brackishWaters
      , Locations.audubonPark
      , Locations.fauborgMarigny
      ]
    )
  , ( Wilderness
    , [ Locations.forgottenMarsh
      , Locations.trappersCabin
      , Locations.twistedUnderbrush
      ]
    )
  , ( Unhallowed
    , [Locations.foulSwamp, Locations.ritualGrounds, Locations.overgrownCairns]
    )
  ]

