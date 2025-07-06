module Arkham.Location.Cards.CosmicGate (cosmicGate) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Arkham.Trait qualified as Trait

newtype CosmicGate = CosmicGate LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicGate :: LocationCard CosmicGate
cosmicGate = locationWith CosmicGate Cards.cosmicGate 1 (Static 1) connectsToAdjacent

instance HasAbilities CosmicGate where
  getAbilities (CosmicGate a) =
    extendRevealed
      a
      [ cosmos a 1
      , forcedAbility a 2 $ Enters #after You (be a)
      , restricted
          a
          3
          ( Here
              <> exists (LocationWithTrait Trait.Void <> not_ (be a))
              <> exists (InvestigatorAt $ be a)
          )
          $ actionAbilityWithCost (ScenarioResourceCost 1)
      ]

instance RunMessage CosmicGate where
  runMessage msg l@(CosmicGate attrs) = runQueueT $ case msg of
    RunCosmos iid lid msgs | lid == toId attrs -> do
      revealedLocations <- select RevealedLocation
      positions <- mapMaybeM findLocationInCosmos revealedLocations
      allEmpty <- concatForM positions \pos ->
        getEmptyPositionsInDirections pos [GridUp, GridDown, GridLeft, GridRight]

      if null allEmpty
        then cosmosFail attrs
        else chooseOrRunOneM iid do
          for_ allEmpty \pos'@(Pos x y) ->
            gridLabeled (cosmicLabel pos') do
              placeCosmos iid attrs (CosmosLocation (Pos x y) lid)
              pushAll msgs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      n <- getSpendableClueCount [iid]
      chooseOrRunOneM iid do
        when (n >= 1) $ labeled "Spend 1 Clue" $ spendClues iid 1
        labeled "Take 1 Horror" $ assignHorror iid (attrs.ability 2) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      investigators <- select $ investigatorAt attrs
      otherLocations <- select $ LocationWithTrait Trait.Void <> not_ (be attrs)
      unless (null otherLocations) do
        chooseSome1M iid "Done moving investigators" do
          targets investigators \investigator -> do
            chooseTargetM iid otherLocations $ moveTo (attrs.ability 3) investigator
      pure l
    _ -> CosmicGate <$> liftRunMessage msg attrs
