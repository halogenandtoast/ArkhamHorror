module Arkham.Agenda.Cards.EmpireOfTheDead
  ( EmpireOfTheDead(..)
  , empireOfTheDead
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Message

newtype EmpireOfTheDead = EmpireOfTheDead AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

empireOfTheDead :: AgendaCard EmpireOfTheDead
empireOfTheDead =
  agenda (1, A) EmpireOfTheDead Cards.empireOfTheDead (Static 6)

instance RunMessage EmpireOfTheDead where
  runMessage msg a@(EmpireOfTheDead attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      specterOfDeath <- EncounterCard
        <$> genEncounterCard Enemies.specterOfDeath
      let specterOfDeathId = EnemyId $ toCardId specterOfDeath
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ pushAll
        [ CreateEnemy specterOfDeath
        , InvestigatorDrawEnemy leadInvestigatorId specterOfDeathId
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
    _ -> EmpireOfTheDead <$> runMessage msg attrs
