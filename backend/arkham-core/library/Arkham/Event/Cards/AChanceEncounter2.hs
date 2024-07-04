module Arkham.Event.Cards.AChanceEncounter2 (aChanceEncounter2, AChanceEncounter2 (..)) where

import Arkham.Capability
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Window (defaultWindows)

newtype AChanceEncounter2 = AChanceEncounter2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aChanceEncounter2 :: EventCard AChanceEncounter2
aChanceEncounter2 = event AChanceEncounter2 Cards.aChanceEncounter2

instance RunMessage AChanceEncounter2 where
  runMessage msg e@(AChanceEncounter2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      discards <-
        select
          $ InDiscardOf (affectsOthers can.have.cards.leaveDiscard)
          <> basic (#ally <> CardWithCost attrs.payment.resources)

      -- Normally we would not error like this, but verifying card costs to
      -- match what is paid is quite difficult. The front-end should just not
      -- update the game state if invalid due to erroring
      when (null discards) (error "Invalid choice")

      focusCards discards \unfocus -> do
        chooseOne
          iid
          [ targetLabel card [PutCardIntoPlay iid card Nothing NoPayment (defaultWindows iid)]
          | card <- discards
          ]
        push unfocus
      pure e
    _ -> AChanceEncounter2 <$> liftRunMessage msg attrs
