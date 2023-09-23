module Arkham.Location.Cards.InfinityOfDarkness (
  infinityOfDarkness,
  InfinityOfDarkness (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Projection
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers

newtype InfinityOfDarkness = InfinityOfDarkness LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infinityOfDarkness :: LocationCard InfinityOfDarkness
infinityOfDarkness =
  locationWith
    InfinityOfDarkness
    Cards.infinityOfDarkness
    2
    (Static 1)
    (connectsToL .~ adjacentLocations)

instance HasAbilities InfinityOfDarkness where
  getAbilities (InfinityOfDarkness attrs) =
    withRevealedAbilities
      attrs
      [ cosmos attrs 1
      , limitedAbility (GroupLimit PerGame 1)
          $ restrictedAbility attrs 2 (Here <> CanMoveThis GridDown)
          $ ActionAbility Nothing (ActionCost 1 <> ScenarioResourceCost 1)
      ]

-- withRevealedAbilities attrs []

instance RunMessage InfinityOfDarkness where
  runMessage msg l@(InfinityOfDarkness attrs) = case msg of
    RunCosmos iid lid msgs | lid == toId attrs -> do
      mpos <- findCosmosPosition iid
      case mpos of
        Nothing -> cosmosFail attrs
        Just pos -> do
          belowChoice <- getEmptyPositionsInDirections pos [GridDown]
          bottommostPositions <- bottommostRevealedLocationPositions

          let
            positionPairs = flip map bottommostPositions $ \pos' ->
              ( pos'
              , [GridUp, GridDown, GridLeft, GridRight]
              )
          emptyPositions <- flip concatMapM positionPairs $ \(pos', dirs) ->
            getEmptyPositionsInDirections pos' dirs

          if null belowChoice && null bottommostPositions
            then cosmosFail attrs
            else
              push
                $ chooseOne
                  iid
                $ [ Label
                    "Connect Below"
                    [ chooseOrRunOne
                        iid
                        [ GridLabel (cosmicLabel pos') (PlaceCosmos iid (toId attrs) (CosmosLocation (Pos x y) lid) : msgs)
                        | pos'@(Pos x y) <- belowChoice
                        ]
                    ]
                  | notNull belowChoice
                  ]
                <> [ Label
                    "Take 2 horror and connect to the bottommost revealed location in a direction of your choice"
                    [ assignDamage iid (toAbilitySource attrs 1) 2
                    , chooseOrRunOne
                        iid
                        [ GridLabel (cosmicLabel pos') (PlaceCosmos iid (toId attrs) (CosmosLocation (Pos x y) lid) : msgs)
                        | pos'@(Pos x y) <- emptyPositions
                        ]
                    ]
                   | notNull emptyPositions
                   ]
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      (map toCard -> cards, _) <- fieldMap InvestigatorDeck (draw 1) iid
      case cards of
        [card] -> do
          cosmos' <- getCosmos
          let mpos = findInCosmos (toId attrs) cosmos'
          case mpos of
            Nothing -> error "location not found in cosmos, we shouldn't be here"
            Just pos -> do
              (emptySpace', placeEmptySpace) <- placeLocationCard Locations.emptySpace
              pushAll
                [ ObtainCard card
                , placeEmptySpace
                , PlaceCosmos iid emptySpace' (EmptySpace pos card)
                , PlaceCosmos iid (toId attrs) (CosmosLocation (updatePosition pos GridDown) (toId attrs))
                ]
        [] -> error "empty deck, what should we do?, maybe don't let this be called?"
        _ -> error "too many cards, why did this happen?"
      pure l
    _ -> InfinityOfDarkness <$> runMessage msg attrs
