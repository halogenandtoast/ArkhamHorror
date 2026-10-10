module Arkham.Homebrew.AgesUnwound.Agendas.DaysNeverBeforeSeen (daysNeverBeforeSeen) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers (tumbleOutOfTime)
import Arkham.Homebrew.AgesUnwound.Sets qualified as Set

newtype DaysNeverBeforeSeen = DaysNeverBeforeSeen AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daysNeverBeforeSeen :: AgendaCard DaysNeverBeforeSeen
daysNeverBeforeSeen = agenda (2, A) DaysNeverBeforeSeen Cards.daysNeverBeforeSeen (Static 4)

instance RunMessage DaysNeverBeforeSeen where
  runMessage msg a@(DaysNeverBeforeSeen attrs) = runQueueT $ case msg of
    {- "Time Turned Dark. Discard each non-weakness treachery card in play. Each
    investigator at an Adrift location moves to a random other Adrift location.
    Shuffle the set-aside Paradox encounter set into the encounter deck, along
    with the encounter discard pile. Place 1 doom on agenda 3a as it is
    revealed." -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      tumbleOutOfTime attrs
      shuffleSetAsideEncounterSet Set.Paradox
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      -- "as it is revealed": the doom lands on the new agenda and can take it
      -- straight past its threshold.
      placeDoomOnAgendaAndCheckAdvance 1
      pure a
    _ -> DaysNeverBeforeSeen <$> liftRunMessage msg attrs
