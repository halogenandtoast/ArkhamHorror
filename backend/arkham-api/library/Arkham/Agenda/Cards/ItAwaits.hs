module Arkham.Agenda.Cards.ItAwaits (
  ItAwaits (..),
  itAwaits,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
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
  runMessage msg a@(ItAwaits attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        ritualSuicideMessages <- commitRitualSuicide attrs
        lead <- getLead
        theWraithRecognizesTheCrucifix <- WornCrucifix `inRecordSet` MementosDiscovered
        investigators <- getInvestigators
        sid <- getRandom
        pushAll
          $ ritualSuicideMessages
          <> [findAndDrawEncounterCard lead (cardIs Treacheries.daemonicPiping)]
          <> ( guard (not theWraithRecognizesTheCrucifix)
                *> [beginSkillTest sid iid attrs iid #willpower (Fixed 4) | iid <- investigators]
             )
          <> [advanceAgendaDeck attrs]
        pure a
      FailedThisSkillTest iid (isSource attrs -> True) -> do
        push $ assignDamageAndHorror iid attrs 1 1
        pure a
      _ -> ItAwaits <$> runMessage msg attrs
