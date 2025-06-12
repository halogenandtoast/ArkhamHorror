module Arkham.Agenda.Cards.ThreeFates (threeFates) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype ThreeFates = ThreeFates AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

threeFates :: AgendaCard ThreeFates
threeFates = agenda (1, A) ThreeFates Cards.threeFates (Static 6)

instance HasAbilities ThreeFates where
  getAbilities (ThreeFates attrs) = [mkAbility attrs 1 $ ActionAbility [#resign] (ActionCost 1)]

instance RunMessage ThreeFates where
  runMessage msg a@(ThreeFates attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      deckCount <- getActDecksInPlayCount
      when (deckCount == 2) $ placeDoomOnAgenda 1
      pure a
    _ -> ThreeFates <$> liftRunMessage msg attrs
