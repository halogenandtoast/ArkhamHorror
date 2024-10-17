module Arkham.Agenda.Cards.FogOnTheBay (FogOnTheBay (..), fogOnTheBay) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Matcher

newtype FogOnTheBay = FogOnTheBay AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fogOnTheBay :: AgendaCard FogOnTheBay
fogOnTheBay =
  agendaWith (1, A) FogOnTheBay Cards.fogOnTheBay (Static 4)
    $ removeDoomMatchersL
    %~ (\rdm -> rdm {removeDoomAgendas = NotAgenda AnyAgenda})

-- Ability is no-op
instance HasAbilities FogOnTheBay where
  getAbilities (FogOnTheBay a) = [mkAbility a 1 $ forced $ AgendaAdvances #when (AgendaWithId a.id)]

instance RunMessage FogOnTheBay where
  runMessage msg a@(FogOnTheBay attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      placeDoomOnAgenda attrs.doom
      pure a
    _ -> FogOnTheBay <$> liftRunMessage msg attrs
