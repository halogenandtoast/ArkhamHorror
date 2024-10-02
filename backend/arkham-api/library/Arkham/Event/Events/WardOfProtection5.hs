module Arkham.Event.Events.WardOfProtection5 (wardOfProtection5, WardOfProtection5 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (cardDrawn)

newtype WardOfProtection5 = WardOfProtection5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wardOfProtection5 :: EventCard WardOfProtection5
wardOfProtection5 = event WardOfProtection5 Cards.wardOfProtection5

instance RunMessage WardOfProtection5 where
  runMessage msg e@(WardOfProtection5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cancelCardEffects attrs (cardDrawn attrs.windows)
      assignHorror iid attrs 1
      pure e
    _ -> WardOfProtection5 <$> liftRunMessage msg attrs
