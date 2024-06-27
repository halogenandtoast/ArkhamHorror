module Arkham.Event.Cards.AgainstAllOdds2 (againstAllOdds2, AgainstAllOdds2 (..)) where

import Arkham.ChaosBag.RevealStrategy
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Event.Runner
import Arkham.Helpers.Window (windowSkillTest)
import Arkham.Modifier
import Arkham.Window qualified as Window

newtype AgainstAllOdds2 = AgainstAllOdds2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

againstAllOdds2 :: EventCard AgainstAllOdds2
againstAllOdds2 = event AgainstAllOdds2 Cards.againstAllOdds2

instance RunMessage AgainstAllOdds2 where
  runMessage msg e@(AgainstAllOdds2 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ (windowSkillTest -> Just st) _ | eid == toId attrs -> do
      n <- getBaseValueDifferenceForSkillTest iid st
      skillTestModifier attrs iid (ChangeRevealStrategy $ RevealAndChoose n 1)
      when (n > 1) $ checkAfter $ Window.CancelledOrIgnoredCardOrGameEffect (toSource attrs)
      pure e
    _ -> AgainstAllOdds2 <$> lift (runMessage msg attrs)
