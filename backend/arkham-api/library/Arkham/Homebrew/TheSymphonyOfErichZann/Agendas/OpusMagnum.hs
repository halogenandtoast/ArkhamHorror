{- | Agenda 3a, Opus Magnum.

Its b side, Coda Ultimatum, is unusual: it becomes *both* the current act and
the current agenda, so advancing it does not continue the agenda deck. The
scenario ends only when every undefeated investigator has resigned (R1).
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.Agendas.OpusMagnum (opusMagnum) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Stories
import Arkham.Helpers.Query (getLead, getSetAsideCard)
import Arkham.Helpers.Story (readStory)

newtype OpusMagnum = OpusMagnum AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

opusMagnum :: AgendaCard OpusMagnum
opusMagnum = agenda (3, A) OpusMagnum Cards.opusMagnum (Static 4)

instance RunMessage OpusMagnum where
  runMessage msg a@(OpusMagnum attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- "Resolve the text on the set aside Beyond the Curtain story card."
      lead <- getLead
      beyondTheCurtain <- getSetAsideCard Stories.beyondTheCurtain
      readStory lead beyondTheCurtain Stories.beyondTheCurtain
      -- Coda Ultimatum stays in play as both act and agenda, so the agenda deck
      -- is deliberately NOT advanced here.
      pure a
    _ -> OpusMagnum <$> liftRunMessage msg attrs
