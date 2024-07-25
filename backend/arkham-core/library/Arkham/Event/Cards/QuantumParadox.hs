module Arkham.Event.Cards.QuantumParadox (quantumParadox, QuantumParadox (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype QuantumParadox = QuantumParadox EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quantumParadox :: EventCard QuantumParadox
quantumParadox = event QuantumParadox Cards.quantumParadox

instance HasAbilities QuantumParadox where
  getAbilities (QuantumParadox x) = [restrictedAbility x 1 InYourHand $ forced $ TurnEnds #when You]

instance RunMessage QuantumParadox where
  runMessage msg e@(QuantumParadox attrs) = runQueueT $ case msg of
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 1) | iid' == iid -> do
      push $ RevealCard $ toCardId attrs
      assignHorror iid (CardSource $ toCard attrs) 1
      pure e
    _ -> QuantumParadox <$> liftRunMessage msg attrs
