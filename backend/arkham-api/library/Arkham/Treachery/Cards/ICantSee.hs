module Arkham.Treachery.Cards.ICantSee (iCantSee, ICantSee (..)) where

import Arkham.Asset.Types (Field (AssetDriver))
import Arkham.Investigator.Types (Field (InvestigatorPlacement))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ICantSee = ICantSee TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iCantSee :: TreacheryCard ICantSee
iCantSee = treachery ICantSee Cards.iCantSee

instance RunMessage ICantSee where
  runMessage msg t@(ICantSee attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      field InvestigatorPlacement iid >>= \case
        InVehicle aid -> do
          driver <- fieldJust AssetDriver aid
          revelationSkillTest sid driver attrs #willpower (Fixed 3)
        _ -> revelationSkillTest sid iid attrs #willpower (Fixed 5)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      field InvestigatorPlacement iid >>= \case
        InVehicle aid -> do
          passengers <- select $ InVehicleMatching $ AssetWithId aid
          for_ passengers \passenger -> assignHorror passenger attrs 2
        _ -> assignHorror iid attrs 3
      pure t
    _ -> ICantSee <$> liftRunMessage msg attrs
