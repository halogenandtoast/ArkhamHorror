module Arkham.Location.Cards.PathwayIntoVoid (pathwayIntoVoid) where

import Arkham.Ability
import Arkham.Card
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers

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
  getAbilities (PathwayIntoVoid a) =
    extendRevealed
      a
      [ cosmos a 1
      , forcedAbility a 2 $ Enters #after You (be a)
      , groupLimit PerRound
          $ restricted a 3 (Here <> oneOf (map CanMoveThis [GridDown, GridUp, GridLeft, GridRight]))
          $ actionAbilityWithCost (ScenarioResourceCost 1)
      ]

instance RunMessage PathwayIntoVoid where
  runMessage msg l@(PathwayIntoVoid attrs) = runQueueT $ case msg of
    RunCosmos iid lid msgs | lid == toId attrs -> do
      mpos <- findCosmosPosition iid
      valids <-
        maybe (pure []) (`getEmptyPositionsInDirections` [GridUp, GridDown, GridLeft, GridRight]) mpos
      chooseCosmos attrs iid valids msgs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      canDiscard <- iid <=~> InvestigatorWithDiscardableCard
      chooseOrRunOneM iid $ withI18n do
        when canDiscard do
          countVar 1 $ labeled' "discardCards" $ chooseAndDiscardCard iid (attrs.ability 2)
        countVar 1 $ labeled' "takeDamage" $ assignDamage iid (attrs.ability 2) 1

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
                  GridUp -> "moveUp"
                  GridDown -> "moveDown"
                  GridLeft -> "moveLeft"
                  GridRight -> "moveRight"

              (emptySpace', placeEmptySpace) <- Msg.placeLocationCard Locations.emptySpace
              chooseOrRunOneM iid $ scenarioI18n do
                for_ validDirections \dir ->
                  labeled' (toGridLabel dir) do
                    obtainCard card.id
                    push placeEmptySpace
                    push $ PlaceCosmos iid emptySpace' (EmptySpace pos card)
                    push $ PlaceCosmos iid (toId attrs) (CosmosLocation (updatePosition pos dir) (toId attrs))
        [] -> error "empty deck, what should we do?, maybe don't let this be called?"
        _ -> error "too many cards, why did this happen?"
      pure l
    Do (PlaceCosmos _ lid cloc) | lid == attrs.id -> do
      handleCosmos lid cloc
      pure l
    _ -> PathwayIntoVoid <$> liftRunMessage msg attrs
