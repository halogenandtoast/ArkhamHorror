module Arkham.Event.Cards.PayYourDue (payYourDue, PayYourDue (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype PayYourDue = PayYourDue EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

payYourDue :: EventCard PayYourDue
payYourDue = event PayYourDue Cards.payYourDue

instance HasAbilities PayYourDue where
  getAbilities (PayYourDue x) = [restrictedAbility x 1 InYourHand $ forced $ TurnEnds #when You]

instance HasModifiersFor PayYourDue where
  getModifiersFor (CardIdTarget cid) (PayYourDue attrs) | toCardId attrs == cid = do
    actions <- field InvestigatorRemainingActions attrs.owner
    modified attrs [CanReduceCostOf (CardWithId cid) (max 0 (actions - 1) * 5)]
  getModifiersFor _ _ = pure []

instance RunMessage PayYourDue where
  runMessage msg e@(PayYourDue attrs) = runQueueT $ case msg of
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 1) | iid' == iid -> do
      push $ RevealCard $ toCardId attrs
      assignDamage iid (CardSource $ toCard attrs) 1
      pure e
    _ -> PayYourDue <$> liftRunMessage msg attrs
