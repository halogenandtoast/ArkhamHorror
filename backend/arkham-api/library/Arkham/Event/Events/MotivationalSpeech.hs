module Arkham.Event.Events.MotivationalSpeech (motivationalSpeech, MotivationalSpeech (..)) where

import Arkham.Card
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher
import Arkham.Window (defaultWindows)

newtype MotivationalSpeech = MotivationalSpeech EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

motivationalSpeech :: EventCard MotivationalSpeech
motivationalSpeech = event MotivationalSpeech Cards.motivationalSpeech

instance RunMessage MotivationalSpeech where
  runMessage msg e@(MotivationalSpeech attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      ts <- select $ affectsOthers $ colocatedWith iid
      chooseOrRunOne iid $ targetLabels ts $ only . handleTargetChoice iid attrs
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      allies <- select $ PlayableCardWithCostReduction NoAction 3 $ inHandOf iid <> #ally
      when (notNull allies) do
        focusCards allies \unfocus -> do
          chooseOneM iid do
            labeled "Do not play ally" $ push unfocus
            targets allies \ally -> do
              push unfocus
              costModifier attrs iid (ReduceCostOf (CardWithId $ toCardId ally) 3)
              push $ PayCardCost iid ally (defaultWindows iid)
      pure e
    _ -> MotivationalSpeech <$> liftRunMessage msg attrs
