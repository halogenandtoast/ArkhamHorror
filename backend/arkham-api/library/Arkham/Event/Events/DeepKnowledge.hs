module Arkham.Event.Events.DeepKnowledge (deepKnowledge) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype DeepKnowledge = DeepKnowledge EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepKnowledge :: EventCard DeepKnowledge
deepKnowledge = event DeepKnowledge Cards.deepKnowledge

instance RunMessage DeepKnowledge where
  runMessage msg e@(DeepKnowledge attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      iids <- select $ affectsOthers $ colocatedWith iid <> can.draw.cards
      chooseInvestigatorAmounts iid "Number of cards to draw" 3 iids attrs
      pure e
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      withInvestigatorAmounts choices \iid n -> drawCards iid (attrs.ability 1) n
      pure e
    _ -> DeepKnowledge <$> liftRunMessage msg attrs
