module Arkham.Event.Cards.WardOfProtection where

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
    PlayThisEvent iid eid | attrs `is` eid -> do
      cancelRevelation attrs (cardDrawn attrs.windows)
      assignHorror iid eid 1
      pure e
    _ -> WardOfProtection <$> liftRunMessage msg attrs
