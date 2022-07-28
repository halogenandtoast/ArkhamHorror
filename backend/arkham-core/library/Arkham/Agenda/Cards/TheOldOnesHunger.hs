module Arkham.Agenda.Cards.TheOldOnesHunger
  ( TheOldOnesHunger(..)
  , theOldOnesHunger
  ) where

import Arkham.Prelude

import Arkham.Agenda.Types
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Message
import Arkham.Scenario.Deck
import Arkham.Target

newtype TheOldOnesHunger = TheOldOnesHunger AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOldOnesHunger :: AgendaCard TheOldOnesHunger
theOldOnesHunger =
  agenda (2, A) TheOldOnesHunger Cards.theOldOnesHunger (Static 6)

instance RunMessage TheOldOnesHunger where
  runMessage msg a@(TheOldOnesHunger attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      scenarioDeckCount <- length <$> getScenarioDeck PotentialSacrifices
      if scenarioDeckCount >= 2
        then a <$ pushAll
          [ DrawRandomFromScenarioDeck
            leadInvestigatorId
            PotentialSacrifices
            (toTarget attrs)
            1
          , AdvanceAgendaDeck agendaDeckId (toSource attrs)
          ]
        else a <$ push (AdvanceAgendaDeck agendaDeckId (toSource attrs))
    DrewFromScenarioDeck _ PotentialSacrifices target cards
      | isTarget attrs target -> a
      <$ push (PlaceUnderneath AgendaDeckTarget cards)
    _ -> TheOldOnesHunger <$> runMessage msg attrs
