module Arkham.Homebrew.ConsternationOnTheConstellation.Agendas.PunishTheInterlopers (punishTheInterlopers) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Agendas qualified as Cards

newtype PunishTheInterlopers = PunishTheInterlopers AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Agenda 3a, the branch reached by advancing act 2. Exhausted locations do not
ready during upkeep. Forced at the start of the enemy phase: discard each Cultist
sharing a location with a Deep One, then move each unengaged Cultist one location
toward whoever controls the Tablet of Dagon.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
punishTheInterlopers :: AgendaCard PunishTheInterlopers
punishTheInterlopers = agenda (3, A) PunishTheInterlopers Cards.punishTheInterlopers (Static 2)

instance RunMessage PunishTheInterlopers where
  runMessage msg (PunishTheInterlopers attrs) = runQueueT $ PunishTheInterlopers <$> liftRunMessage msg attrs
