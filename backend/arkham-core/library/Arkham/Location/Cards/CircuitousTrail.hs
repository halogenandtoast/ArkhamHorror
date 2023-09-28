module Arkham.Location.Cards.CircuitousTrail (
  circuitousTrail,
  CircuitousTrail (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Data.Semigroup

newtype CircuitousTrail = CircuitousTrail LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

circuitousTrail :: LocationCard CircuitousTrail
circuitousTrail = location CircuitousTrail Cards.circuitousTrail 1 (PerPlayer 1)

instance HasModifiersFor CircuitousTrail where
  getModifiersFor (AbilityTarget iid ab) (CircuitousTrail attrs) = do
    here <- iid `isAt` attrs
    anyWithCompass <- getAny <$> selectAgg (Any . elem Compass) InvestigatorSupplies (colocatedWith iid)
    case abilityAction ab of
      Just Action.Investigate ->
        pure $ toModifiers attrs [AdditionalCost (ResourceCost 3) | here, not anyWithCompass]
      Just Action.Explore ->
        pure $ toModifiers attrs [AdditionalCost (ResourceCost 3) | here, not anyWithCompass]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage CircuitousTrail where
  runMessage msg (CircuitousTrail attrs) = CircuitousTrail <$> runMessage msg attrs
