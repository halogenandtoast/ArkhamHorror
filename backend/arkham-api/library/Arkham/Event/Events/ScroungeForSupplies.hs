module Arkham.Event.Events.ScroungeForSupplies (scroungeForSupplies) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype ScroungeForSupplies = ScroungeForSupplies EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scroungeForSupplies :: EventCard ScroungeForSupplies
scroungeForSupplies = event ScroungeForSupplies Cards.scroungeForSupplies

instance RunMessage ScroungeForSupplies where
  runMessage msg e@(ScroungeForSupplies attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <- select $ inDiscardOf iid <> basic.level 0
      chooseTargetM iid cards $ addToHand iid . only
      pure e
    _ -> ScroungeForSupplies <$> liftRunMessage msg attrs
