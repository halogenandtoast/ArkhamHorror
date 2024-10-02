module Arkham.Event.Events.WardOfProtection2 (wardOfProtection2, WardOfProtection2 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (cardDrawn)

newtype WardOfProtection2 = WardOfProtection2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wardOfProtection2 :: EventCard WardOfProtection2
wardOfProtection2 = event WardOfProtection2 Cards.wardOfProtection2

instance RunMessage WardOfProtection2 where
  runMessage msg e@(WardOfProtection2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cancelRevelation attrs (cardDrawn attrs.windows)
      assignHorror iid attrs 1
      pure e
    _ -> WardOfProtection2 <$> liftRunMessage msg attrs
