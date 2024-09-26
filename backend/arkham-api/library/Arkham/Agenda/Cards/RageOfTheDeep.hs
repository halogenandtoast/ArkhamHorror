module Arkham.Agenda.Cards.RageOfTheDeep
  ( RageOfTheDeep(..)
  , rageOfTheDeep
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype RageOfTheDeep = RageOfTheDeep AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rageOfTheDeep :: AgendaCard RageOfTheDeep
rageOfTheDeep = agenda (4, A) RageOfTheDeep Cards.rageOfTheDeep (Static 12)

instance RunMessage RageOfTheDeep where
  runMessage msg a@(RageOfTheDeep attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> RageOfTheDeep <$> liftRunMessage msg attrs
