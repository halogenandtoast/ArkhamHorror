module Arkham.Treachery.Cards.TidalAlignment (tidalAlignment, TidalAlignment (..)) where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.FloodLevel
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TidalAlignment = TidalAlignment TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tidalAlignment :: TreacheryCard TidalAlignment
tidalAlignment = treachery TidalAlignment Cards.tidalAlignment

instance RunMessage TidalAlignment where
  runMessage msg t@(TidalAlignment attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      choices <- select $ LocationWithInvestigator Anyone
      chooseTargetM iid choices $ handleTarget iid attrs
      pure t
    HandleTargetChoice _iid (isSource attrs -> True) (LocationTarget lid) -> do
      selectEach (investigatorAt lid) \iid' -> assignDamage iid' attrs 1
      isDesolateCoastline <- lid <=~> locationIs Locations.desolateCoastline
      fl <- getFloodLevel lid
      when (fl == FullyFlooded || isDesolateCoastline) $ gainSurge attrs
      pure t
    _ -> TidalAlignment <$> liftRunMessage msg attrs
