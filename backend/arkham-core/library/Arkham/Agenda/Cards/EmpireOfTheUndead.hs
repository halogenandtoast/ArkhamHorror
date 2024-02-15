module Arkham.Agenda.Cards.EmpireOfTheUndead (
  EmpireOfTheUndead (..),
  empireOfTheUndead,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher hiding (InvestigatorDefeated)

newtype EmpireOfTheUndead = EmpireOfTheUndead AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

empireOfTheUndead :: AgendaCard EmpireOfTheUndead
empireOfTheUndead =
  agenda (2, A) EmpireOfTheUndead Cards.empireOfTheUndead (Static 12)

instance RunMessage EmpireOfTheUndead where
  runMessage msg a@(EmpireOfTheUndead attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      investigatorIds <- select UneliminatedInvestigator
      a
        <$ pushAll
          ( [InvestigatorDefeated (toSource attrs) iid | iid <- investigatorIds]
              <> [SufferTrauma iid 1 0 | iid <- investigatorIds]
          )
    _ -> EmpireOfTheUndead <$> runMessage msg attrs
