module Arkham.Homebrew.ConsternationOnTheConstellation.Agendas.Captured (captured) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Agendas qualified as Cards

newtype Captured = Captured AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Agenda 1a. Each copy of Order Enforcer gets +1 health per investigator and
gains aloof and Elite; investigators cannot play or put into play Item assets,
nor leave Cargo Room. Advancing throws everyone into Open Water, puts the
set-aside locations into play and advances the act deck to 2a.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
captured :: AgendaCard Captured
captured = agenda (1, A) Captured Cards.captured (Static 2)

instance RunMessage Captured where
  runMessage msg (Captured attrs) = runQueueT $ Captured <$> liftRunMessage msg attrs
