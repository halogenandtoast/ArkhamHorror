module Arkham.Agenda.Cards.UnchangingAsTheSea (
  UnchangingAsTheSea (..),
  unchangingAsTheSea,
) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Matcher

newtype UnchangingAsTheSea = UnchangingAsTheSea AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unchangingAsTheSea :: AgendaCard UnchangingAsTheSea
unchangingAsTheSea =
  agendaWith (2, A) UnchangingAsTheSea Cards.unchangingAsTheSea (Static 7)
    $ removeDoomMatchersL
    %~ (\rdm -> rdm {removeDoomAgendas = NotAgenda AnyAgenda})

-- Ability is no-op
instance HasAbilities UnchangingAsTheSea where
  getAbilities (UnchangingAsTheSea a) = [mkAbility a 2 $ forced $ AgendaAdvances #when (AgendaWithId a.id)]

instance RunMessage UnchangingAsTheSea where
  runMessage msg a@(UnchangingAsTheSea attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      placeDoomOnAgenda attrs.doom
      pure a
    _ -> UnchangingAsTheSea <$> liftRunMessage msg attrs
