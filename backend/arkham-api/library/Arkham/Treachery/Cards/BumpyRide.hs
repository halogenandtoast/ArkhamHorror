module Arkham.Treachery.Cards.BumpyRide (bumpyRide, BumpyRide (..)) where

import Arkham.Asset.Types (Field (AssetDriver))
import Arkham.Investigator.Types (Field (InvestigatorPlacement))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BumpyRide = BumpyRide TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bumpyRide :: TreacheryCard BumpyRide
bumpyRide = treachery BumpyRide Cards.bumpyRide

instance RunMessage BumpyRide where
  runMessage msg t@(BumpyRide attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      field InvestigatorPlacement iid >>= \case
        InVehicle aid -> do
          driver <- fieldJust AssetDriver aid
          revelationSkillTest sid driver attrs #agility (Fixed 3)
        _ -> revelationSkillTest sid iid attrs #agility (Fixed 5)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      field InvestigatorPlacement iid >>= \case
        InVehicle aid -> do
          passengers <- select $ InVehicleMatching $ AssetWithId aid
          for_ passengers \passenger -> assignDamage passenger attrs 2
        _ -> assignDamage iid attrs 3
      pure t
    _ -> BumpyRide <$> liftRunMessage msg attrs
