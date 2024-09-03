module Arkham.Location.Cards.PathwayIntoVoid (
  pathwayIntoVoid,
  PathwayIntoVoid (..),
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
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Arkham.Timing qualified as Timing

newtype PathwayIntoVoid = PathwayIntoVoid LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pathwayIntoVoid :: LocationCard PathwayIntoVoid
pathwayIntoVoid =
  locationWith
    PathwayIntoVoid
    Cards.pathwayIntoVoid
    4
    (Static 2)
    (connectsToL .~ adjacentLocations)

instance HasAbilities PathwayIntoVoid where
  getAbilities (PathwayIntoVoid attrs) =
    withRevealedAbilities
      attrs
      [ cosmos attrs 1
      , forcedAbility attrs 2 $ Enters Timing.After You $ LocationWithId (toId attrs)
      , limitedAbility (GroupLimit PerRound 1)
          $ restrictedAbility
            attrs
            3
            ( Here
                <> AnyCriterion [CanMoveThis GridDown, CanMoveThis GridUp, CanMoveThis GridLeft, CanMoveThis GridRight]
            )
          $ ActionAbility [] (ActionCost 1 <> ScenarioResourceCost 1)
      ]

instance RunMessage PathwayIntoVoid where
  runMessage msg l@(PathwayIntoVoid attrs) = case msg of
    RunCosmos iid lid msgs | lid == toId attrs -> do
      mpos <- findCosmosPosition iid
      valids <-
        maybe (pure []) (`getEmptyPositionsInDirections` [GridUp, GridDown, GridLeft, GridRight]) mpos
      if null valids
        then cosmosFail attrs
        else do
          player <- getPlayer iid
          push
            $ chooseOne
              player
              [ GridLabel (cosmicLabel pos') (PlaceCosmos iid (toId attrs) (CosmosLocation (Pos x y) lid) : msgs)
              | pos'@(Pos x y) <- valids
              ]
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      canDiscard <- iid <=~> InvestigatorWithDiscardableCard
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ Label
            "Discard 1 card from your hand"
            [toMessage $ chooseAndDiscardCard iid (toAbilitySource attrs 2)]
          | canDiscard
          ]
        <> [Label "Take 1 Damage" [assignDamage iid (toAbilitySource attrs 2) 1]]

      pure l
    UseCardAbility iid (isSource attrs -> True) 3 _ _ -> do
      (map toCard -> cards, _) <- fieldMap InvestigatorDeck (draw 1) iid
      case cards of
        [card] -> do
          cosmos' <- getCosmos
          let mpos = findInCosmos (toId attrs) cosmos'
          case mpos of
            Nothing -> error "location not found in cosmos, we shouldn't be here"
            Just pos -> do
              validDirections <- flip filterM [GridUp, GridDown, GridLeft, GridRight] $ \dir ->
                isNothing <$> getLocationInDirection pos dir

              let
                toGridLabel = \case
                  GridUp -> "Move Up"
                  GridDown -> "Move Down"
                  GridLeft -> "Move Left"
                  GridRight -> "Move Right"

              (emptySpace', placeEmptySpace) <- placeLocationCard Locations.emptySpace
              player <- getPlayer iid
              push
                $ chooseOrRunOne player
                $ [ Label
                    (toGridLabel dir)
                    [ ObtainCard card
                    , placeEmptySpace
                    , PlaceCosmos iid emptySpace' (EmptySpace pos card)
                    , PlaceCosmos iid (toId attrs) (CosmosLocation (updatePosition pos dir) (toId attrs))
                    ]
                  | dir <- validDirections
                  ]
        [] -> error "empty deck, what should we do?, maybe don't let this be called?"
        _ -> error "too many cards, why did this happen?"
      pure l
    _ -> PathwayIntoVoid <$> runMessage msg attrs
