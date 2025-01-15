module Arkham.Event.Events.DarkMemory (darkMemory) where

import Arkham.Ability.Scripted.Builder
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype DarkMemory = DarkMemory EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasCardCode, HasCardDef, IsCard, Sourceable)
  deriving HasAbilities via Scripted DarkMemory

darkMemory :: EventCard DarkMemory
darkMemory = event DarkMemory Cards.darkMemory

instance ScriptedAbilities DarkMemory where
  scriptedAbilities = abilities do
    forced (TurnEnds #when You) do
      inYourHand
      run do
        revealCard this
        assignHorror you (toCardId this) 2

instance RunMessage DarkMemory where
  runMessage = withScripted $ onPlay $ placeDoomOnAgendaAndCheckAdvance 1
