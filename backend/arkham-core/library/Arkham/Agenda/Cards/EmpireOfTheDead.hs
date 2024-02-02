module Arkham.Agenda.Cards.EmpireOfTheDead (
  EmpireOfTheDead (..),
  empireOfTheDead,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.GameValue
import Arkham.Placement

newtype EmpireOfTheDead = EmpireOfTheDead AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

empireOfTheDead :: AgendaCard EmpireOfTheDead
empireOfTheDead =
  agenda (1, A) EmpireOfTheDead Cards.empireOfTheDead (Static 6)

instance RunMessage EmpireOfTheDead where
  runMessage msg a@(EmpireOfTheDead attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      lead <- getLead
      specterOfDeath <- genCard Enemies.specterOfDeath
      createSpecterOfDeath <- createEnemy specterOfDeath Unplaced
      pushAll
        [ toMessage createSpecterOfDeath
        , InvestigatorDrawEnemy lead (enemyCreationEnemyId createSpecterOfDeath)
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> EmpireOfTheDead <$> runMessage msg attrs
