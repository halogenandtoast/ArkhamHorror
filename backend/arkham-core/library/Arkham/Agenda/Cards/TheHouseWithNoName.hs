module Arkham.Agenda.Cards.TheHouseWithNoName (TheHouseWithNoName (..), theHouseWithNoName) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.GameValue
import Arkham.Prelude

newtype TheHouseWithNoName = TheHouseWithNoName AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHouseWithNoName :: AgendaCard TheHouseWithNoName
theHouseWithNoName = agenda (1, A) TheHouseWithNoName Cards.theHouseWithNoName (Static 5)

instance RunMessage TheHouseWithNoName where
  runMessage msg a@(TheHouseWithNoName attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      theUnnamable <- genCard Enemies.theUnnamable
      createTheUnnamable <- toMessage <$> createEnemy theUnnamable SpawnViaSpawnInstruction
      pushAll [createTheUnnamable, advanceAgendaDeck attrs]
      pure a
    _ -> TheHouseWithNoName <$> runMessage msg attrs
