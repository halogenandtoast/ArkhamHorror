module Arkham.Event.Cards.Guidance (guidance, Guidance (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype Guidance = Guidance EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guidance :: EventCard Guidance
guidance = event Guidance Cards.guidance

instance RunMessage Guidance where
  runMessage msg e@(Guidance attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      selectOneToHandle iid attrs $ NotYou <> InvestigatorAt YourLocation <> YetToTakeTurn
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget investigator) -> do
      push $ GainActions investigator (toSource attrs) 1
      pure e
    _ -> Guidance <$> liftRunMessage msg attrs
