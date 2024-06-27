module Arkham.Event.Cards.CloseCall2 (closeCall2, CloseCall2 (..)) where

import Arkham.Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (evadedEnemy)

newtype CloseCall2 = CloseCall2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeCall2 :: EventCard CloseCall2
closeCall2 = event CloseCall2 Cards.closeCall2

instance RunMessage CloseCall2 where
  runMessage msg e@(CloseCall2 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent _iid (is attrs -> True) _ (evadedEnemy -> enemyId) _ -> do
      shuffleIntoDeck EncounterDeck enemyId
      pure e
    _ -> CloseCall2 <$> lift (runMessage msg attrs)
