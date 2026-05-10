module Arkham.Event.Events.MotivationalSpeech (motivationalSpeech) where

import Arkham.Card
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher

newtype MotivationalSpeech = MotivationalSpeech EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

motivationalSpeech :: EventCard MotivationalSpeech
motivationalSpeech = event MotivationalSpeech Cards.motivationalSpeech

instance RunMessage MotivationalSpeech where
  runMessage msg e@(MotivationalSpeech attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      ts <-
        select (affectsColocated iid)
          >>= filterM
            (\iid' -> selectAny $ PlayableCardWithCostReduction NoAction 3 $ inHandOf ForPlay iid' <> #ally)

      chooseOrRunOneM iid $ targets ts $ handleTarget iid attrs
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      allies <- select $ PlayableCardWithCostReduction NoAction 3 $ inHandOf ForPlay iid <> #ally
      when (notNull allies) do
        focusCards allies do
          chooseOneM iid do
            labeledI "doNotPlayAlly" unfocusCards
            targets allies \ally -> do
              unfocusCards
              costModifier attrs iid (ReduceCostOf (CardWithId $ toCardId ally) 3)
              playCardPayingCost iid ally
      pure e
    _ -> MotivationalSpeech <$> liftRunMessage msg attrs
