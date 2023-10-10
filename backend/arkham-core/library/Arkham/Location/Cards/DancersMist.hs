module Arkham.Location.Cards.DancersMist (
  dancersMist,
  DancersMist (..),
)
where

import Arkham.Prelude

import Arkham.Direction
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Projection
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Arkham.Timing qualified as Timing
import Data.List qualified as List

newtype DancersMist = DancersMist LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dancersMist :: LocationCard DancersMist
dancersMist =
  locationWith
    DancersMist
    Cards.dancersMist
    3
    (Static 2)
    (connectsToL .~ adjacentLocations)

instance HasAbilities DancersMist where
  getAbilities (DancersMist attrs) =
    withRevealedAbilities
      attrs
      [ cosmos attrs 1
      , restrictedAbility attrs 2 (LocationExists AccessibleLocation)
          $ ReactionAbility
            (Moves Timing.After You AnySource Anywhere $ LocationWithId $ toId attrs)
            (ScenarioResourceCost 1)
      ]

instance RunMessage DancersMist where
  runMessage msg l@(DancersMist attrs) = case msg of
    RunCosmos iid lid msgs | lid == toId attrs -> do
      mpos <- findCosmosPosition iid
      case mpos of
        Nothing -> cosmosFail attrs
        Just pos -> do
          let
          rightChoice <- getEmptyPositionsInDirections pos [GridRight]
          directionsWithLocations <-
            filterM (fmap isJust . getLocationInDirection pos) [GridUp, GridDown, GridLeft, GridRight]
          let
            positionPairs = flip map directionsWithLocations $ \dir ->
              ( updatePosition pos dir
              , List.delete (oppositeDirection dir) [GridUp, GridDown, GridLeft, GridRight]
              )
          emptyPositions <- flip concatMapM positionPairs $ \(pos', dirs) ->
            getEmptyPositionsInDirections pos' dirs

          if null emptyPositions && null rightChoice
            then cosmosFail attrs
            else do
              player <- getPlayer iid
              push
                $ chooseOne
                  player
                $ [ Label
                    "Connect to the Right"
                    [ chooseOrRunOne
                        player
                        [ GridLabel (cosmicLabel pos') (PlaceCosmos iid (toId attrs) (CosmosLocation (Pos x y) lid) : msgs)
                        | pos'@(Pos x y) <- rightChoice
                        ]
                    ]
                  | notNull rightChoice
                  ]
                <> [ Label
                    "Lose 2 resources and connect to an adjacent location in a direction of your choice"
                    [ LoseResources iid (toAbilitySource attrs 1) 2
                    , chooseOrRunOne
                        player
                        [ GridLabel (cosmicLabel pos') (PlaceCosmos iid (toId attrs) (CosmosLocation (Pos x y) lid) : msgs)
                        | pos'@(Pos x y) <- emptyPositions
                        ]
                    ]
                   | notNull emptyPositions
                   ]
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      startId <- fieldJust InvestigatorLocation iid
      accessibleLocationIds <- selectList $ AccessibleFrom $ LocationWithId startId
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel lid [Move $ move attrs iid lid]
          | lid <- accessibleLocationIds
          ]
      pure l
    _ -> DancersMist <$> runMessage msg attrs
