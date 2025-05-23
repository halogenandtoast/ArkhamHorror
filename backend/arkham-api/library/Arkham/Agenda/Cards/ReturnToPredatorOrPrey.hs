module Arkham.Agenda.Cards.ReturnToPredatorOrPrey (returnToPredatorOrPrey) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies

newtype ReturnToPredatorOrPrey = ReturnToPredatorOrPrey AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToPredatorOrPrey :: AgendaCard ReturnToPredatorOrPrey
returnToPredatorOrPrey = agenda (1, A) ReturnToPredatorOrPrey Cards.returnToPredatorOrPrey (Static 6)

instance HasAbilities ReturnToPredatorOrPrey where
  getAbilities (ReturnToPredatorOrPrey attrs) =
    [mkAbility attrs 1 $ ActionAbility [Action.Resign] (ActionCost 1)]

instance RunMessage ReturnToPredatorOrPrey where
  runMessage msg a@(ReturnToPredatorOrPrey attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      createEnemyEngagedWithPrey_ =<< genCard Enemies.narogath
      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure a
    _ -> ReturnToPredatorOrPrey <$> liftRunMessage msg attrs
