module Arkham.Location.Cards.TightTurn_c (tightTurn_c, TightTurn_c (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype TightTurn_c = TightTurn_c LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tightTurn_c :: LocationCard TightTurn_c
tightTurn_c =
  locationWith TightTurn_c Cards.tightTurn_c 2 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities TightTurn_c where
  getAbilities (TightTurn_c a) =
    extendRevealed
      a
      [ mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)
      , mkAbility a 2 $ forced $ VehicleLeaves #when (notSeenVehicle a) (be a)
      ]

instance RunMessage TightTurn_c where
  runMessage msg l@(TightTurn_c attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 (getLeavingVehicle -> vehicle) _ -> do
      n <- handleVehicleLeaves vehicle attrs.id 2
      inVehicle <- select $ InVehicleMatching (AssetWithId vehicle)
      replicateM_ n $ chooseTargetM iid inVehicle $ handleTarget iid (attrs.ability 2)
      pure . TightTurn_c $ attrs & globalMetaL %~ sawVehicle vehicle
    HandleTargetChoice _iid (isAbilitySource attrs 2 -> True) (InvestigatorTarget iid) -> do
      assignDamage iid (attrs.ability 2) 2
      pure l
    _ -> TightTurn_c <$> liftRunMessage msg attrs
