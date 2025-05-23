module Arkham.Agenda.Cards.PredatorOrPrey (predatorOrPrey) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies

newtype PredatorOrPrey = PredatorOrPrey AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

predatorOrPrey :: AgendaCard PredatorOrPrey
predatorOrPrey = agenda (1, A) PredatorOrPrey Cards.predatorOrPrey (Static 6)

instance HasAbilities PredatorOrPrey where
  getAbilities (PredatorOrPrey attrs) = [mkAbility attrs 1 $ ActionAbility [#resign] (ActionCost 1)]

instance RunMessage PredatorOrPrey where
  runMessage msg a@(PredatorOrPrey attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      theMaskedHunter <- genCard Enemies.theMaskedHunter
      createEnemyEngagedWithPrey_ theMaskedHunter
      advanceAgendaDeck attrs
      pure a
    _ -> PredatorOrPrey <$> liftRunMessage msg attrs
