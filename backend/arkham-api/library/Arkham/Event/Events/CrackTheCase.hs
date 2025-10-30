module Arkham.Event.Events.CrackTheCase (crackTheCase) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (selectAffectsColocated)
import Arkham.Helpers.Location
import Arkham.Location.Types (Field (..))
import Arkham.Projection

newtype CrackTheCase = CrackTheCase EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crackTheCase :: EventCard CrackTheCase
crackTheCase = event CrackTheCase Cards.crackTheCase

instance RunMessage CrackTheCase where
  runMessage msg e@(CrackTheCase attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid \lid -> do
        iids <- selectAffectsColocated iid can.gain.resources
        shroud <- fieldMap LocationShroud (fromMaybe 0) lid
        repeated shroud $ chooseOrRunOneM iid $ for_ iids \iid' ->
          resourceLabeled iid' $ gainResources iid' attrs 1
      pure e
    _ -> CrackTheCase <$> liftRunMessage msg attrs
