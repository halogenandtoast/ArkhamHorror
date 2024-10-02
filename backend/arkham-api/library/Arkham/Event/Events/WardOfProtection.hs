module Arkham.Event.Events.WardOfProtection where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (cardDrawn)

newtype WardOfProtection = WardOfProtection EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wardOfProtection :: EventCard WardOfProtection
wardOfProtection = event WardOfProtection Cards.wardOfProtection

instance RunMessage WardOfProtection where
  runMessage msg e@(WardOfProtection attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cancelRevelation attrs (cardDrawn attrs.windows)
      assignHorror iid attrs 1
      pure e
    _ -> WardOfProtection <$> liftRunMessage msg attrs
