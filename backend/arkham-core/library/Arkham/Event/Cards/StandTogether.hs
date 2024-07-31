module Arkham.Event.Cards.StandTogether (standTogether, StandTogether (..)) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype StandTogether = StandTogether EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

standTogether :: EventCard StandTogether
standTogether = event StandTogether Cards.standTogether

instance RunMessage StandTogether where
  runMessage msg e@(StandTogether attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectWhenNotNull (notInvestigator iid <> colocatedWith iid) \investigators -> do
        chooseOrRunOneM iid do
          for_ investigators \iid' -> do
            targeting iid' do
              gainResourcesIfCan iid (toSource attrs) 2
              whenM (can.affect.otherPlayers iid) do
                gainResourcesIfCan iid' (toSource attrs) 2
      pure e
    _ -> StandTogether <$> liftRunMessage msg attrs
