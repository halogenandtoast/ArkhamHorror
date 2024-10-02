module Arkham.Event.Events.Forewarned1 (forewarned1, Forewarned1 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (cardDrawn)

newtype Forewarned1 = Forewarned1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forewarned1 :: EventCard Forewarned1
forewarned1 = event Forewarned1 Cards.forewarned1

instance RunMessage Forewarned1 where
  runMessage msg e@(Forewarned1 attrs) = runQueueT case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      push $ InvestigatorPlaceCluesOnLocation iid (toSource attrs) 1
      cancelRevelation attrs (cardDrawn attrs.windows)
      pure e
    _ -> Forewarned1 <$> liftRunMessage msg attrs
