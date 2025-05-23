module Arkham.Event.Events.MomentOfRespite3 (momentOfRespite3) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator

newtype MomentOfRespite3 = MomentOfRespite3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

momentOfRespite3 :: EventCard MomentOfRespite3
momentOfRespite3 = event MomentOfRespite3 Cards.momentOfRespite3

instance RunMessage MomentOfRespite3 where
  runMessage msg e@(MomentOfRespite3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      whenM (canHaveHorrorHealed attrs iid) $ healHorror iid attrs 3
      drawCards iid attrs 1
      pure e
    _ -> MomentOfRespite3 <$> liftRunMessage msg attrs
