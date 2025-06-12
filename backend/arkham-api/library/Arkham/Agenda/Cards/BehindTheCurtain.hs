module Arkham.Agenda.Cards.BehindTheCurtain (behindTheCurtain) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype BehindTheCurtain = BehindTheCurtain AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

behindTheCurtain :: AgendaCard BehindTheCurtain
behindTheCurtain = agenda (2, A) BehindTheCurtain Cards.behindTheCurtain (Static 6)

instance HasAbilities BehindTheCurtain where
  getAbilities (BehindTheCurtain attrs) = [mkAbility attrs 1 $ ActionAbility [#resign] (ActionCost 1)]

instance RunMessage BehindTheCurtain where
  runMessage msg a@(BehindTheCurtain attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      deckCount <- getActDecksInPlayCount
      when (deckCount == 2) $ placeDoomOnAgenda 1
      when (deckCount == 1) $ placeDoomOnAgenda 2
      pure a
    _ -> BehindTheCurtain <$> liftRunMessage msg attrs
