module Arkham.Agenda.Cards.TheFinalCountdown (
  TheFinalCountdown (..),
  theFinalCountdown,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheFinalCountdown = TheFinalCountdown AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFinalCountdown :: AgendaCard TheFinalCountdown
theFinalCountdown =
  agendaWith (3, A) TheFinalCountdown Cards.theFinalCountdown (Static 8)
    $ removeDoomMatchersL
    %~ (\m -> m {removeDoomEnemies = NotEnemy AnyEnemy})

instance RunMessage TheFinalCountdown where
  runMessage msg a@(TheFinalCountdown attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        ritualSuicideMessages <- commitRitualSuicide attrs
        lead <- getLead
        theCreatureRecognizesYou <- CornHuskDoll `inRecordSet` MementosDiscovered
        azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
        pushAll
          $ ritualSuicideMessages
          <> [findAndDrawEncounterCard lead (cardIs Treacheries.daemonicPiping)]
          <> [RemoveDoom (toSource attrs) (toTarget azathoth) 1 | theCreatureRecognizesYou]
          <> [advanceAgendaDeck attrs]
        pure a
      _ -> TheFinalCountdown <$> runMessage msg attrs
