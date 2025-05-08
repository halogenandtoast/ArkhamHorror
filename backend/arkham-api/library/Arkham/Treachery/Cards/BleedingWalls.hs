module Arkham.Treachery.Cards.BleedingWalls (bleedingWalls) where

import Arkham.Calculation
import Arkham.Location.Types (Field (..))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BleedingWalls = BleedingWalls TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bleedingWalls :: TreacheryCard BleedingWalls
bleedingWalls = treachery BleedingWalls Cards.bleedingWalls

instance RunMessage BleedingWalls where
  runMessage msg t@(BleedingWalls attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest
        sid
        iid
        attrs
        #willpower
        (InvestigatorLocationMaybeFieldCalculation iid LocationShroud)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      loseActions iid attrs 1
      assignHorror iid attrs 1
      pure t
    _ -> BleedingWalls <$> liftRunMessage msg attrs
