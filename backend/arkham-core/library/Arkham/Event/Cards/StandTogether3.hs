module Arkham.Event.Cards.StandTogether3 (standTogether3, StandTogether3 (..)) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype StandTogether3 = StandTogether3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

standTogether3 :: EventCard StandTogether3
standTogether3 = event StandTogether3 Cards.standTogether3

instance RunMessage StandTogether3 where
  runMessage msg e@(StandTogether3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectWhenNotNull (notInvestigator iid <> colocatedWith iid) \investigators -> do
        chooseOrRunOneM iid do
          for_ investigators $ \iid' -> do
            targeting iid' do
              drawCardsIfCan iid (toSource attrs) 2
              gainResourcesIfCan iid (toSource attrs) 2
              whenM (can.affect.otherPlayers iid) do
                drawCardsIfCan iid' (toSource attrs) 2
                gainResourcesIfCan iid' (toSource attrs) 2
      pure e
    _ -> StandTogether3 <$> liftRunMessage msg attrs
