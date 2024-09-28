module Arkham.Location.Cards.FishStreetBridgeInTooDeep (
  fishStreetBridgeInTooDeep,
  FishStreetBridgeInTooDeep (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Helpers.Scenario
import Arkham.Ability
import Arkham.SortedPair
import Arkham.Helpers.Modifiers (modified, ModifierType(..))
import Arkham.Matcher
import Arkham.Scenarios.InTooDeep.Helpers
import Data.Map.Strict qualified as Map

newtype FishStreetBridgeInTooDeep = FishStreetBridgeInTooDeep LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fishStreetBridgeInTooDeep :: LocationCard FishStreetBridgeInTooDeep
fishStreetBridgeInTooDeep =
  locationWith
    FishStreetBridgeInTooDeep
    Cards.fishStreetBridgeInTooDeep
    1
    (PerPlayer 2)
    connectsToAdjacent

instance HasModifiersFor FishStreetBridgeInTooDeep where
  getModifiersFor target (FishStreetBridgeInTooDeep a) | isTarget a target = do
    Meta meta <- getScenarioMeta
    let x = sum [n| (inSortedPair a.id -> True, n) <- Map.toList meta]
    modified a [ShroudModifier x | x > 0]
  getModifiersFor _ _ = pure []

instance HasAbilities FishStreetBridgeInTooDeep where
  getAbilities (FishStreetBridgeInTooDeep a) =
    extendRevealed a [restrictedAbility a 1 (Here <> thisIs a LocationWithAdjacentBarrier) $ parleyAction $ AddCurseTokenCost 2]

instance RunMessage FishStreetBridgeInTooDeep where
  runMessage msg l@(FishStreetBridgeInTooDeep attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      removeBarrierBetweenConnected iid attrs.id
      pure l
    _ -> FishStreetBridgeInTooDeep <$> liftRunMessage msg attrs
