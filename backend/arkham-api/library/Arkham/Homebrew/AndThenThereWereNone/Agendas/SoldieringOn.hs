{- | Agenda 1, Soldiering On.

"{wild} icons on Method and Victim cards do not count as {willpower},
{intellect}, {combat}, or {agility} icons" needs no modifier: every tally in
this scenario is plain icon equality (see
'Arkham.Homebrew.AndThenThereWereNone.Helpers.victimMatchingIcons'), so a
{wild} icon already matches only {wild}, and nothing in the set asks the engine
whether one icon counts as another.

Advancing is the failure clock: everyone resigns, so no resolution is reached
and the scenario reads that as Resolution 1.
-}
module Arkham.Homebrew.AndThenThereWereNone.Agendas.SoldieringOn (soldieringOn) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Agendas qualified as Cards

newtype SoldieringOn = SoldieringOn AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

soldieringOn :: AgendaCard SoldieringOn
soldieringOn = agenda (1, A) SoldieringOn Cards.soldieringOn (Static 16)

instance RunMessage SoldieringOn where
  runMessage msg a@(SoldieringOn attrs) = runQueueT $ case msg of
    -- "Each investigator who has not resigned immediately resigns."
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator resign
      pure a
    _ -> SoldieringOn <$> liftRunMessage msg attrs
