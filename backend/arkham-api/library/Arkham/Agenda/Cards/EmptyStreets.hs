module Arkham.Agenda.Cards.EmptyStreets (emptyStreets) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Spawn

newtype EmptyStreets = EmptyStreets AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emptyStreets :: AgendaCard EmptyStreets
emptyStreets = agenda (2, A) EmptyStreets Cards.emptyStreets (Static 7)

instance HasModifiersFor EmptyStreets where
  getModifiersFor (EmptyStreets a) = do
    modifySelect a AnyEnemy [ForceSpawn SpawnAtRandomLocation]

instance RunMessage EmptyStreets where
  runMessage msg a@(EmptyStreets attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator (`sufferPhysicalTrauma` 1)
      razin <- selectAny $ InPlayEnemy $ enemyIs Enemies.razinFarhiReanimatedArtificer
      push $ if razin then R1 else R2
      pure a
    _ -> EmptyStreets <$> liftRunMessage msg attrs
