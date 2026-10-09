{- | Agenda 3a, Opus Magnum.

Its back is Coda Ultimatum, which stays in play as both act and agenda. That is
an agenda of its own here, so advancing this one hands over to it -- see
"Arkham.Homebrew.TheSymphonyOfErichZann.Agendas.CodaUltimatum".
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.Agendas.OpusMagnum (opusMagnum) where

import Arkham.Agenda.Import.Lifted
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Helpers.Query (getLead, getSetAsideCard)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Stories
import Arkham.Matcher

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
      {- "Then, replace the current Act and Agenda with this Coda Ultimatum. It
      is now both the current act and agenda." Discarding an act empties the act
      stack, so nothing follows it; Coda Ultimatum carries the act's objective
      itself. -}
      selectEach AnyAct $ toDiscard attrs
      {- The deck is already mid-advance, and the windows for it have been
      checked, so this hands over without going through `AdvanceToAgenda` and
      checking them a second time. -}
      push $ Do (AdvanceToAgenda attrs.deck Cards.codaUltimatum Agenda.A (toSource attrs))
      pure a
    _ -> OpusMagnum <$> liftRunMessage msg attrs
