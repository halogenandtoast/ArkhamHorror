module Arkham.Agenda.Cards.ItAwaits (itAwaits) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Matcher
import Arkham.Helpers.Log (inRecordSet)
import Arkham.Helpers.Query (getLead)
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype ItAwaits = ItAwaits AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

itAwaits :: AgendaCard ItAwaits
itAwaits =
  agendaWith (2, A) ItAwaits Cards.itAwaits (Static 6)
    $ removeDoomMatchersL
    %~ (\m -> m {removeDoomEnemies = NotEnemy AnyEnemy})

instance RunMessage ItAwaits where
  runMessage msg a@(ItAwaits attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead

      commitRitualSuicide attrs
      findAndDrawEncounterCard lead (cardIs Treacheries.daemonicPiping)

      theWraithRecognizesTheCrucifix <- WornCrucifix `inRecordSet` MementosDiscovered
      unless theWraithRecognizesTheCrucifix do
        eachInvestigator \iid -> do
          sid <- genId
          beginSkillTest sid iid attrs iid #willpower (Fixed 4)
      advanceAgendaDeck attrs
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamageAndHorror iid attrs 1 1
      pure a
    _ -> ItAwaits <$> liftRunMessage msg attrs
