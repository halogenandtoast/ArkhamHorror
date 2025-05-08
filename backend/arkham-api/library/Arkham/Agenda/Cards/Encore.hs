module Arkham.Agenda.Cards.Encore (encore) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Matcher

newtype Encore = Encore AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encore :: AgendaCard Encore
encore = agenda (2, A) Encore Cards.encore (Static 6)

instance HasAbilities Encore where
  getAbilities (Encore a) = [mkAbility a 1 $ forced $ AddedToVictory #after $ cardIs Cards.royalEmissary]

instance RunMessage Encore where
  runMessage msg a@(Encore attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ RemoveAllDoomFromPlay defaultRemoveDoomMatchers
      push $ ResetAgendaDeckToStage 1
      placeDoomOnAgenda 3
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> assignHorror iid attrs 100
      pure a
    _ -> Encore <$> liftRunMessage msg attrs
