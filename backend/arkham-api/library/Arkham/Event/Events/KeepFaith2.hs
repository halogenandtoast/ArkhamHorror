module Arkham.Event.Events.KeepFaith2 (keepFaith2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.ChaosBag

newtype KeepFaith2 = KeepFaith2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keepFaith2 :: EventCard KeepFaith2
keepFaith2 = event KeepFaith2 Cards.keepFaith2

instance RunMessage KeepFaith2 where
  runMessage msg e@(KeepFaith2 attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      n <- min 4 <$> getRemainingBlessTokens
      repeated n $ addChaosToken #bless
      pure e
    _ -> KeepFaith2 <$> liftRunMessage msg attrs
