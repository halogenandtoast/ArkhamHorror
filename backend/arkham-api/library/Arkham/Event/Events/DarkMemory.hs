module Arkham.Event.Events.DarkMemory (darkMemory) where

import Arkham.Ability hiding (you)
import Arkham.Script
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype DarkMemory = DarkMemory EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasCardCode, HasCardDef, IsCard)

darkMemory :: EventCard DarkMemory
darkMemory = event DarkMemory Cards.darkMemory

instance HasAbilities DarkMemory where
  getAbilities (DarkMemory x) = [restricted x 1 InYourHand $ forced $ TurnEnds #when You]

instance RunMessage DarkMemory where
  runMessage = script do
    inHand $ onAbility 1 do
      revealCard this
      assignHorror you (toCardId this) 2
    onPlay $ placeDoomOnAgendaAndCheckAdvance 1
