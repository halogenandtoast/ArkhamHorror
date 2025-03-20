module Arkham.Location.Cards.InfinityOfDarkness (infinityOfDarkness) where

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
    extendRevealed
      attrs
      [ cosmos attrs 1
      , groupLimit PerGame
          $ restricted attrs 2 (Here <> CanMoveThis GridDown)
          $ actionAbilityWithCost (ScenarioResourceCost 1)
      ]

instance RunMessage InfinityOfDarkness where
  runMessage msg l@(InfinityOfDarkness attrs) = runQueueT $ case msg of
    RunCosmos iid lid msgs | lid == toId attrs -> do
      findCosmosPosition iid >>= \case
        Nothing -> cosmosFail attrs
        Just pos -> do
          belowChoice <- getEmptyPositionsInDirections pos [GridDown]
          bottommostPositions <- bottommostRevealedLocationPositions

          let positionPairs = map (,[GridUp ..]) bottommostPositions
          emptyPositions <- concatMapM (uncurry getEmptyPositionsInDirections) positionPairs

          if null belowChoice && null bottommostPositions
            then cosmosFail attrs
            else do
              chooseOneM iid do
                questionLabeled "Choose where to connect"
                questionLabeledCard attrs
                when (notNull belowChoice) do
                  labeled "Connect Below" do
                    chooseOrRunOneM iid do
                      for_ belowChoice \pos'@(Pos x y) -> do
                        gridLabeled (cosmicLabel pos')
                          $ pushAll
                          $ PlaceCosmos iid (toId attrs) (CosmosLocation (Pos x y) lid)
                          : msgs
                when (notNull emptyPositions) do
                  labeled
                    "Take 2 damage and connect to the bottommost revealed location in a direction of your choice"
                    do
                      assignDamage iid (attrs.ability 1) 2
                      chooseOrRunOneM iid do
                        for_ emptyPositions \pos'@(Pos x y) -> do
                          gridLabeled (cosmicLabel pos')
                            $ pushAll
                            $ PlaceCosmos iid (toId attrs) (CosmosLocation (Pos x y) lid)
                            : msgs
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
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
              push $ PlaceCosmos iid (toId attrs) (CosmosLocation (updatePosition pos GridDown) (toId attrs))
        [] -> error "empty deck, what should we do?, maybe don't let this be called?"
        _ -> error "too many cards, why did this happen?"
      pure l
    _ -> InfinityOfDarkness <$> liftRunMessage msg attrs
