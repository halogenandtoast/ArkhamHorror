module Arkham.Scenarios.APhantomOfTruth.Helpers where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher

investigatorsNearestToTheOrganist ::
  (HasList (InvestigatorId, Distance) env EnemyMatcher, MonadReader env m) =>
  m (Distance, [InvestigatorId])
investigatorsNearestToTheOrganist = do
  mappings :: [(InvestigatorId, Distance)] <-
    getList
      ( EnemyOneOf
          [ enemyIs Cards.theOrganistHopelessIDefiedHim
          , enemyIs Cards.theOrganistDrapedInMystery
          ]
      )
  let minDistance :: Int =
        fromJustNote "error" . minimumMay $ map (unDistance . snd) mappings
  pure . (Distance minDistance,) . hashNub . map fst $
    filter
      ((== minDistance) . unDistance . snd)
      mappings
