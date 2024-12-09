module Arkham.Event.Events.DarkMemory (darkMemory) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype DarkMemory = DarkMemory EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkMemory :: EventCard DarkMemory
darkMemory = event DarkMemory Cards.darkMemory

instance HasAbilities DarkMemory where
  getAbilities (DarkMemory x) = [restricted x 1 InYourHand $ forced $ TurnEnds #when You]

instance RunMessage DarkMemory where
  runMessage msg e@(DarkMemory attrs) = runQueueT $ case msg of
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 1) | iid' == iid -> do
      push $ RevealCard attrs.cardId
      assignHorror iid attrs.cardId 2
      pure e
    PlayThisEvent _ eid | attrs `is` eid -> do
      placeDoomOnAgendaAndCheckAdvance 1
      pure e
    _ -> DarkMemory <$> liftRunMessage msg attrs
