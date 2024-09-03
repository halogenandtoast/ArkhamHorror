module Arkham.Scenarios.APhantomOfTruth.Helpers (
  module Arkham.Scenarios.APhantomOfTruth.Helpers,
  module X,
) where

import Arkham.Prelude

import Arkham.Campaigns.ThePathToCarcosa.Helpers as X
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Movement
import Arkham.Source

getTheOrganist :: HasGame m => m EnemyId
getTheOrganist = selectJust $ EnemyWithTitle "The Organist"

moveOrganistAwayFromNearestInvestigator :: HasGame m => m Message
moveOrganistAwayFromNearestInvestigator = do
  organist <- getTheOrganist
  lead <- getLeadPlayer
  lids <- select $ ConnectedFrom (LocationWithInvestigator $ NearestToEnemy $ EnemyWithId organist)
  emptyLids <- filterM (<=~> LocationWithoutInvestigators) lids

  let targets = if notNull emptyLids then emptyLids else lids
  pure
    $ chooseOrRunOne
      lead
      [targetLabel lid [EnemyMove organist lid] | lid <- targets]

disengageEachEnemyAndMoveToConnectingLocation
  :: (HasGame m, Sourceable source) => source -> m [Message]
disengageEachEnemyAndMoveToConnectingLocation source = do
  lead <- getLeadPlayer
  iids <- getInvestigatorIds
  enemyPairs <-
    forToSnd
      iids
      (select . EnemyIsEngagedWith . InvestigatorWithId)
  locationPairs <- for iids $ \iid -> do
    locations <- select $ AccessibleFrom $ LocationWithInvestigator $ InvestigatorWithId iid
    player <- getPlayer iid
    pure (iid, player, locations)
  pure
    $ [ DisengageEnemy iid enemy
      | (iid, enemies) <- enemyPairs
      , enemy <- enemies
      ]
    <> [ chooseOneAtATime
          lead
          [ targetLabel
            iid
            [ chooseOne
                player
                $ Label "Do not move" []
                : [ targetLabel lid [Move $ move source iid lid]
                  | lid <- locations
                  ]
            ]
          | (iid, player, locations) <- locationPairs
          ]
       ]
