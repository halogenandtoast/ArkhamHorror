module Arkham.Event.Cards.DarkMemoryAdvanced where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher

newtype DarkMemoryAdvanced = DarkMemoryAdvanced EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkMemoryAdvanced :: EventCard DarkMemoryAdvanced
darkMemoryAdvanced = event DarkMemoryAdvanced Cards.darkMemoryAdvanced

instance HasAbilities DarkMemoryAdvanced where
  getAbilities (DarkMemoryAdvanced x) = [restrictedAbility x 1 InYourHand $ ForcedAbility $ TurnEnds #when You]

instance RunMessage DarkMemoryAdvanced where
  runMessage msg e@(DarkMemoryAdvanced attrs) = case msg of
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 1) | iid' == iid -> do
      pushAll
        [ RevealCard $ toCardId attrs
        , assignHorror iid (CardSource $ toCard attrs) 2
        ]
      pure e
    PlayThisEvent _ eid | attrs `is` eid -> do
      push placeDoomOnAgendaAndCheckAdvance
      pure e
    _ -> DarkMemoryAdvanced <$> runMessage msg attrs
