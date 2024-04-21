module Arkham.Event.Cards.CheatTheSystem1 (cheatTheSystem1, CheatTheSystem1 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Calculation
import Arkham.Matcher

newtype CheatTheSystem1 = CheatTheSystem1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cheatTheSystem1 :: EventCard CheatTheSystem1
cheatTheSystem1 = event CheatTheSystem1 Cards.cheatTheSystem1

instance RunMessage CheatTheSystem1 where
  runMessage msg e@(CheatTheSystem1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      gainResourcesIfCan iid attrs
        =<< calculate (DifferentClassAmong $ ControlledBy $ InvestigatorWithId iid)
      pure e
    _ -> CheatTheSystem1 <$> lift (runMessage msg attrs)
