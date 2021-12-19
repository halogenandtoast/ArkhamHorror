module Arkham.Scenarios.APhantomOfTruth.Helpers where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher hiding (MoveAction)
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target

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

moveOrganistAwayFromNearestInvestigator ::
  ( MonadReader env m
  , HasId LeadInvestigatorId env ()
  , HasId LocationId env EnemyId
  , HasList (LocationId, Distance) env InvestigatorId
  , HasList (InvestigatorId, Distance) env EnemyMatcher
  , Query LocationMatcher env
  , Query EnemyMatcher env
  ) =>
  m Message
moveOrganistAwayFromNearestInvestigator = do
  organist <-
    selectJust $
      EnemyOneOf
        [ enemyIs Cards.theOrganistDrapedInMystery
        , enemyIs Cards.theOrganistHopelessIDefiedHim
        ]
  from <- getId organist
  leadInvestigatorId <- getLeadInvestigatorId
  (minDistance, iids) <- investigatorsNearestToTheOrganist
  lids <-
    setFromList . concat
      <$> for
        iids
        ( \iid -> do
            rs <- getList iid
            pure $ map fst $ filter ((> minDistance) . snd) rs
        )
  withNoInvestigators <- select LocationWithoutInvestigators
  let forced = lids `intersect` withNoInvestigators
      targets = toList $ if null forced then lids else forced
  pure $
    chooseOne
      leadInvestigatorId
      [ TargetLabel (LocationTarget lid) [EnemyMove organist from lid]
      | lid <- targets
      ]

disengageEachEnemyAndMoveToConnectingLocation ::
  ( MonadReader env m
  , HasSet InvestigatorId env ()
  , HasId LeadInvestigatorId env ()
  , Query EnemyMatcher env
  , Query LocationMatcher env
  ) =>
  m [Message]
disengageEachEnemyAndMoveToConnectingLocation = do
  leadInvestigatorId <- getLeadInvestigatorId
  iids <- getInvestigatorIds
  enemyPairs <-
    traverse
      (traverseToSnd (selectList . EnemyIsEngagedWith . InvestigatorWithId))
      iids
  locationPairs <-
    traverse
      ( traverseToSnd
          ( selectList
              . AccessibleFrom
              . LocationWithInvestigator
              . InvestigatorWithId
          )
      )
      iids
  pure $
    [ DisengageEnemy iid enemy
    | (iid, enemies) <- enemyPairs
    , enemy <- enemies
    ]
      <> [ chooseOneAtATime
            leadInvestigatorId
            [ TargetLabel
              (InvestigatorTarget iid)
              [ chooseOne
                  iid
                  [ TargetLabel
                    (LocationTarget lid)
                    [MoveAction iid lid Free False]
                  | lid <- locations
                  ]
              ]
            | (iid, locations) <- locationPairs
            ]
         ]
