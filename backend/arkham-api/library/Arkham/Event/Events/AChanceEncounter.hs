module Arkham.Event.Events.AChanceEncounter (aChanceEncounter) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher

newtype AChanceEncounter = AChanceEncounter EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aChanceEncounter :: EventCard AChanceEncounter
aChanceEncounter = event AChanceEncounter Cards.aChanceEncounter

instance RunMessage AChanceEncounter where
  runMessage msg e@(AChanceEncounter attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      discards <- select $ #ally <> InDiscardOf (affectsOthers can.have.cards.leaveDiscard)
      focusCards discards do
        chooseTargetM iid discards \card -> do
          putCardIntoPlay iid card
          atEndOfRound attrs $ push $ Msg.toDiscard attrs card.id
      pure e
    _ -> AChanceEncounter <$> liftRunMessage msg attrs
