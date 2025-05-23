module Arkham.Event.Events.CrypticResearch4 (crypticResearch4) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype CrypticResearch4 = CrypticResearch4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticResearch4 :: EventCard CrypticResearch4
crypticResearch4 = event CrypticResearch4 Cards.crypticResearch4

instance RunMessage CrypticResearch4 where
  runMessage msg e@(CrypticResearch4 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      investigators <- select $ affectsOthers $ colocatedWith iid <> can.draw.cards
      chooseTargetM iid investigators \iid' -> drawCards iid' attrs 3
      pure e
    _ -> CrypticResearch4 <$> liftRunMessage msg attrs
