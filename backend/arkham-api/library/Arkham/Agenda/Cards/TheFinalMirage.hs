module Arkham.Agenda.Cards.TheFinalMirage (theFinalMirage) where

-- NOTE: This is just a placeholder for lookup purposes, this card is implemented as an Act card

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheFinalMirage = TheFinalMirage AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFinalMirage :: AgendaCard TheFinalMirage
theFinalMirage = agendaWith (6, A) TheFinalMirage Cards.theFinalMirage (Static 0) (doomThresholdL .~ Nothing)

instance RunMessage TheFinalMirage where
  runMessage msg (TheFinalMirage attrs) = TheFinalMirage <$> runMessage msg attrs
