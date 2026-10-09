{- | Agenda 3a, Opus Magnum.

Its b side, Coda Ultimatum, is unusual: it becomes *both* the current act and
the current agenda, so advancing it does not continue the agenda deck. The
scenario ends only when every undefeated investigator has resigned (R1).
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.Agendas.OpusMagnum (opusMagnum) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getLead, getSetAsideCard)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Stories
import Arkham.Matcher

newtype OpusMagnum = OpusMagnum AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Coda Ultimatum prints no doom value -- it is not an agenda that advances,
it is the thing you are trying to escape. Doom still lands on it, which is what
the Window to Nothingness watches for, so without this the mythos threshold
check would try to advance it the moment it reached Opus Magnum's printed 4.
-}
instance HasModifiersFor OpusMagnum where
  getModifiersFor (OpusMagnum a) =
    when (onSide B a) $ modifySelf a [CannotBeAdvancedByDoomThreshold]

{- | Coda Ultimatum's "Objective - Save yourself! If each undefeated
investigator has resigned, (→R1)."

It only exists on the b side, where this card is standing in for the act as well
as the agenda, so the objective has to be offered here rather than left to the
engine's own "everyone is eliminated" path -- which would end the scenario out
from under the card instead of through its objective.
-}
instance HasAbilities OpusMagnum where
  getAbilities (OpusMagnum a) =
    guard (onSide B a)
      *> [ onlyOnce
             $ restricted a 1 AllUndefeatedInvestigatorsResigned
             $ Objective
             $ forced AnyWindow
         ]

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
      stack, so nothing follows it; this card stays in play as the agenda and
      carries the act's objective itself. The agenda deck is deliberately not
      advanced. -}
      selectEach AnyAct $ toDiscard attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R1
      pure a
    _ -> OpusMagnum <$> liftRunMessage msg attrs
