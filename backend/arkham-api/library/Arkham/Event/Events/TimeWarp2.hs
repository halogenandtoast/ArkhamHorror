module Arkham.Event.Events.TimeWarp2 (timeWarp2) where

import Arkham.Card
import Arkham.Event.Types (Field(..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Projection

newtype TimeWarp2 = TimeWarp2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeWarp2 :: EventCard TimeWarp2
timeWarp2 = event TimeWarp2 Cards.timeWarp2

instance RunMessage TimeWarp2 where
  runMessage msg e@(TimeWarp2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      -- we need to extract the payments
      card <- field EventCard attrs.id
      let pc = fromJustNote "has to be a player card" $ preview _PlayerCard card
      -- when we undo action timewarp will either be in hand or on the deck so
      -- we just remove it from the game to find it no matter where it is Then
      -- we add it directly to the discard
      push UndoAction
      push $ RemovePlayerCardFromGame True card
      push $ AddToDiscard iid pc
      pushAll $ eventPaymentMessages attrs
      pure e
    _ -> TimeWarp2 <$> liftRunMessage msg attrs
