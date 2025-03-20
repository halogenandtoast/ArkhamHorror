module Arkham.Location.Cards.FlightIntoOblivion (flightIntoOblivion) where

import Arkham.Ability
import Arkham.Card
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers

newtype FlightIntoOblivion = FlightIntoOblivion LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flightIntoOblivion :: LocationCard FlightIntoOblivion
flightIntoOblivion =
  locationWith
    FlightIntoOblivion
    Cards.flightIntoOblivion
    2
    (Static 1)
    (connectsToL .~ adjacentLocations)

instance HasAbilities FlightIntoOblivion where
  getAbilities (FlightIntoOblivion attrs) =
    extendRevealed
      attrs
      [ cosmos attrs 1
      , groupLimit PerGame
          $ restricted attrs 2 (Here <> CanMoveThis GridUp)
          $ actionAbilityWithCost (ScenarioResourceCost 1)
      ]

instance RunMessage FlightIntoOblivion where
  runMessage msg l@(FlightIntoOblivion attrs) = runQueueT $ case msg of
    RunCosmos iid lid msgs | lid == toId attrs -> do
      findCosmosPosition iid >>= \case
        Nothing -> cosmosFail attrs
        Just pos -> do
          aboveChoice <- getEmptyPositionsInDirections pos [GridUp]
          topmostPositions <- topmostRevealedLocationPositions

          let positionPairs = map (,[GridUp ..]) topmostPositions
          emptyPositions <- concatMapM (uncurry getEmptyPositionsInDirections) positionPairs

          if null aboveChoice && null topmostPositions
            then cosmosFail attrs
            else do
              chooseOneM iid do
                questionLabeled "Choose where to connect"
                questionLabeledCard attrs
                when (notNull aboveChoice) do
                  labeled "Connect Above" do
                    chooseOrRunOneM iid do
                      for_ aboveChoice \pos'@(Pos x y) -> do
                        gridLabeled (cosmicLabel pos')
                          $ pushAll
                          $ PlaceCosmos iid (toId attrs) (CosmosLocation (Pos x y) lid)
                          : msgs
                when (notNull emptyPositions) do
                  labeled "Take 2 horror and connect to the topmost revealed location in a direction of your choice" do
                    assignHorror iid (toAbilitySource attrs 1) 2
                    chooseOrRunOneM iid do
                      for_ emptyPositions \pos'@(Pos x y) -> do
                        gridLabeled (cosmicLabel pos')
                          $ pushAll
                          $ PlaceCosmos iid (toId attrs) (CosmosLocation (Pos x y) lid)
                          : msgs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      (map toCard -> cards, _) <- fieldMap InvestigatorDeck (draw 1) iid
      case cards of
        [card] -> do
          cosmos' <- getCosmos
          case findInCosmos (toId attrs) cosmos' of
            Nothing -> error "location not found in cosmos, we shouldn't be here"
            Just pos -> do
              obtainCard card
              emptySpace' <- placeLocationCard Locations.emptySpace
              push $ PlaceCosmos iid emptySpace' (EmptySpace pos card)
              push $ PlaceCosmos iid (toId attrs) (CosmosLocation (updatePosition pos GridUp) (toId attrs))
        [] -> error "empty deck, what should we do?, maybe don't let this be called?"
        _ -> error "too many cards, why did this happen?"
      pure l
    _ -> FlightIntoOblivion <$> liftRunMessage msg attrs
