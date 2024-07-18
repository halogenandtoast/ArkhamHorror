module Arkham.Event.Cards.Guidance1 (guidance1, Guidance1 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype Guidance1 = Guidance1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guidance1 :: EventCard Guidance1
guidance1 = event Guidance1 Cards.guidance1

instance RunMessage Guidance1 where
  runMessage msg e@(Guidance1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ NotYou <> InvestigatorAt YourLocation <> YetToTakeTurn
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      turnModifiers iid attrs iid [SkillModifier sType 1 | sType <- [minBound ..]]
      push $ GainActions iid (toSource attrs) 1
      pure e
    _ -> Guidance1 <$> liftRunMessage msg attrs
