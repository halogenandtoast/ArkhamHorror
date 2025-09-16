{-# OPTIONS_GHC -Wno-deprecations #-}

module Arkham.Location.Cards.DancersMist (dancersMist) where

import Arkham.Ability
import Arkham.Direction
import Arkham.ForMovement
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Data.List qualified as List

newtype DancersMist = DancersMist LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dancersMist :: LocationCard DancersMist
dancersMist = locationWith DancersMist Cards.dancersMist 3 (Static 2) (connectsToL .~ adjacentLocations)

instance HasAbilities DancersMist where
  getAbilities (DancersMist a) =
    extendRevealed
      a
      [ cosmos a 1
      , restricted a 2 (exists AccessibleLocation)
          $ triggered (Moves #after You AnySource Anywhere (be a)) (ScenarioResourceCost 1)
      ]

instance RunMessage DancersMist where
  runMessage msg l@(DancersMist attrs) = runQueueT $ case msg of
    RunCosmos iid lid msgs | lid == toId attrs -> do
      findCosmosPosition iid >>= \case
        Nothing -> cosmosFail attrs
        Just pos -> do
          rightChoice <- getEmptyPositionsInDirections pos [GridRight]
          directionsWithLocations <- filterM (fmap isJust . getLocationInDirection pos) [GridUp ..]
          let
            positionPairs = flip map directionsWithLocations \dir ->
              ( updatePosition pos dir
              , List.delete (oppositeDirection dir) [GridUp ..]
              )
          emptyPositions <- traceShowId <$> concatMapM (uncurry getEmptyPositionsInDirections) (traceShowId positionPairs)

          if null emptyPositions && null rightChoice
            then cosmosFail attrs
            else do
              chooseOneM iid do
                when (notNull rightChoice) do
                  labeled "Connect to the Right" do
                    chooseOrRunOneM iid do
                      for_ rightChoice \pos'@(Pos x y) -> do
                        gridLabeled (cosmicLabel pos')
                          $ pushAll
                          $ PlaceCosmos iid (toId attrs) (CosmosLocation (Pos x y) lid)
                          : msgs

                when (notNull emptyPositions) do
                  labeled "Lose 2 resources and connect to an adjacent location in a direction of your choice" do
                    push $ LoseResources iid (toAbilitySource attrs 1) 2
                    chooseOrRunOneM iid do
                      for_ emptyPositions \pos'@(Pos x y) -> do
                        gridLabeled (cosmicLabel pos')
                          $ pushAll
                          $ PlaceCosmos iid (toId attrs) (CosmosLocation (Pos x y) lid)
                          : msgs
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      startId <- fieldJust InvestigatorLocation iid
      accessibleLocationIds <- select $ accessibleFrom ForMovement startId
      chooseTargetM iid accessibleLocationIds $ moveTo attrs iid
      pure l
    _ -> DancersMist <$> liftRunMessage msg attrs
