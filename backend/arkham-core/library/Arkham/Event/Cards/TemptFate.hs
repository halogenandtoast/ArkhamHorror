module Arkham.Event.Cards.TemptFate ( temptFate, TemptFate (..),) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.ChaosBag

newtype TemptFate = TemptFate EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temptFate :: EventCard TemptFate
temptFate = event TemptFate Cards.temptFate

instance RunMessage TemptFate where
  runMessage msg e@(TemptFate attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      b <- min 3 <$> getRemainingBlessTokens
      c <- min 3 <$> getRemainingCurseTokens
      pushAll $ replicate b (AddChaosToken #bless)
      addCurseTokens c
      drawCardsIfCan iid attrs 1
      pure e
    _ -> TemptFate <$> runMessage msg attrs
