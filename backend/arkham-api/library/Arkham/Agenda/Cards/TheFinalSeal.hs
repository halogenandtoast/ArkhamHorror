module Arkham.Agenda.Cards.TheFinalSeal (theFinalSeal) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheFinalSeal = TheFinalSeal AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFinalSeal :: AgendaCard TheFinalSeal
theFinalSeal = agenda (2, A) TheFinalSeal Cards.theFinalSeal (Static 10)

-- TODO: abilities

instance RunMessage TheFinalSeal where
  runMessage msg (TheFinalSeal attrs) = runQueueT $ TheFinalSeal <$> liftRunMessage msg attrs
