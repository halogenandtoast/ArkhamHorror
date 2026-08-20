module Arkham.Location.Cards.TheInnsmouthConspiracy.InTooDeep.FishStreetBridge (
  fishStreetBridge,
  FishStreetBridge (..),
)
where

import Arkham.Location.CardDefs.TheInnsmouthConspiracy.InTooDeep qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Helpers.Scenario
import Arkham.Ability
import Arkham.SortedPair
import Arkham.Helpers.Modifiers (modifySelf, ModifierType(..))
import Arkham.Matcher
import Arkham.Scenarios.TheInnsmouthConspiracy.InTooDeep.Helpers
import Data.Map.Strict qualified as Map

newtype FishStreetBridge = FishStreetBridge LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fishStreetBridge :: LocationCard FishStreetBridge
fishStreetBridge =
  locationWith
    FishStreetBridge
    Cards.fishStreetBridge
    1
    (PerPlayer 2)
    connectsToAdjacent

instance HasModifiersFor FishStreetBridge where
  getModifiersFor (FishStreetBridge a) = do
    Meta meta <- getScenarioMeta
    let x = sum [n| (inSortedPair a.id -> True, n) <- Map.toList meta]
    modifySelf a [ShroudModifier x | x > 0]

instance HasAbilities FishStreetBridge where
  getAbilities (FishStreetBridge a) =
    extendRevealed a [restrictedAbility a 1 (Here <> thisIs a LocationWithAdjacentBarrier) $ parleyAction $ AddCurseTokenCost 2]

instance RunMessage FishStreetBridge where
  runMessage msg l@(FishStreetBridge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      removeBarrierBetweenConnected iid attrs.id
      pure l
    _ -> FishStreetBridge <$> liftRunMessage msg attrs
