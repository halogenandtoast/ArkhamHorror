module Arkham.Location.Cards.TightTurn_b (tightTurn_b, TightTurn_b (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype TightTurn_b = TightTurn_b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tightTurn_b :: LocationCard TightTurn_b
tightTurn_b =
  locationWith TightTurn_b Cards.tightTurn_b 3 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities TightTurn_b where
  getAbilities (TightTurn_b a) =
    extendRevealed
      a
      [ mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)
      , mkAbility a 2 $ forced $ VehicleLeaves #when (notSeenVehicle a) (be a)
      ]

instance RunMessage TightTurn_b where
  runMessage msg l@(TightTurn_b attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 (getLeavingVehicle -> vehicle) _ -> do
      n <- handleVehicleLeaves vehicle attrs.id 2
      inVehicle <- select $ InVehicleMatching (AssetWithId vehicle)
      replicateM_ n $ chooseTargetM iid inVehicle $ handleTarget iid (attrs.ability 2)
      pure . TightTurn_b $ attrs & globalMetaL %~ sawVehicle vehicle
    HandleTargetChoice _iid (isAbilitySource attrs 2 -> True) (InvestigatorTarget iid) -> do
      assignDamage iid (attrs.ability 2) 1
      pure l
    _ -> TightTurn_b <$> liftRunMessage msg attrs
