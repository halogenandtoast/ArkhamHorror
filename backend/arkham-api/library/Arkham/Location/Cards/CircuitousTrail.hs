module Arkham.Location.Cards.CircuitousTrail (circuitousTrail, CircuitousTrail (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Helpers.Location (isAt)
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CircuitousTrail = CircuitousTrail LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

circuitousTrail :: LocationCard CircuitousTrail
circuitousTrail = location CircuitousTrail Cards.circuitousTrail 1 (PerPlayer 1)

instance HasModifiersFor CircuitousTrail where
  getModifiersFor (CircuitousTrail attrs) =
    fromMaybe mempty <$> runMaybeT do
      iid <- MaybeT $ selectOne ActiveInvestigator
      liftGuardM $ iid `isAt` attrs
      liftGuardM $ selectNone $ InvestigatorWithSupply Compass <> colocatedWith iid
      abilities <- lift $ select $ mapOneOf AbilityIsAction [#investigate, #explore]
      modifyEach attrs (map (AbilityTarget iid) abilities) [AdditionalCost (ResourceCost 3)]

instance RunMessage CircuitousTrail where
  runMessage msg (CircuitousTrail attrs) = CircuitousTrail <$> runMessage msg attrs
