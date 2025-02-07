module Arkham.Event.Events.KeepFaith (keepFaith) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.ChaosBag

newtype KeepFaith = KeepFaith EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keepFaith :: EventCard KeepFaith
keepFaith = event KeepFaith Cards.keepFaith

instance RunMessage KeepFaith where
  runMessage msg e@(KeepFaith attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      n <- min 4 <$> getRemainingBlessTokens
      repeated n $ addChaosToken #bless
      pure e
    _ -> KeepFaith <$> liftRunMessage msg attrs
