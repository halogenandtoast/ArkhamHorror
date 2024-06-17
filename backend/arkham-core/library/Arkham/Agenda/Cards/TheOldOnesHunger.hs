module Arkham.Agenda.Cards.TheOldOnesHunger (TheOldOnesHunger (..), theOldOnesHunger) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Prelude
import Arkham.Scenario.Deck

newtype TheOldOnesHunger = TheOldOnesHunger AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOldOnesHunger :: AgendaCard TheOldOnesHunger
theOldOnesHunger = agenda (2, A) TheOldOnesHunger Cards.theOldOnesHunger (Static 6)

instance RunMessage TheOldOnesHunger where
  runMessage msg a@(TheOldOnesHunger attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      lead <- getLeadInvestigatorId
      scenarioDeckCount <- length <$> getScenarioDeck PotentialSacrifices
      if scenarioDeckCount >= 2
        then
          pushAll
            [ DrawCards lead $ randomTargetCardDraw attrs PotentialSacrifices 1
            , AdvanceAgendaDeck agendaDeckId (toSource attrs)
            ]
        else push (AdvanceAgendaDeck agendaDeckId (toSource attrs))
      pure a
    DrewCards _iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      push $ PlaceUnderneath AgendaDeckTarget drewCards.cards
      pure a
    _ -> TheOldOnesHunger <$> runMessage msg attrs
