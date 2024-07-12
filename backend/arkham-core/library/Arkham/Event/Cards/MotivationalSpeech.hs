module Arkham.Event.Cards.MotivationalSpeech (motivationalSpeech, MotivationalSpeech (..)) where

import Arkham.Card
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Modifiers qualified as Msg
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
      targets <- select $ affectsOthers $ colocatedWith iid
      chooseOrRunOne iid $ targetLabels targets $ only . handleTargetChoice iid attrs
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      allies <-
        select $ PlayableCardWithCostReduction NoAction 3 $ InHandOf (InvestigatorWithId iid) <> #ally
      when (notNull allies) do
        focusCards allies \unfocus -> do
          chooseOne iid $ Label "Do not play ally" [unfocus]
            : [ targetLabel
                ally
                [ unfocus
                , Msg.costModifier attrs iid (ReduceCostOf (CardWithId $ toCardId ally) 3)
                , PayCardCost iid ally (defaultWindows iid)
                ]
              | ally <- allies
              ]
      pure e
    _ -> MotivationalSpeech <$> liftRunMessage msg attrs
