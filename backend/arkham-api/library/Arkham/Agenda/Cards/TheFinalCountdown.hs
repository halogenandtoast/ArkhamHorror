module Arkham.Agenda.Cards.TheFinalCountdown (theFinalCountdown) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log (inRecordSet)
import Arkham.Helpers.Query (getLead)
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
  runMessage msg a@(TheFinalCountdown attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      commitRitualSuicide attrs
      findAndDrawEncounterCard lead (cardIs Treacheries.daemonicPiping)

      theCreatureRecognizesYou <- CornHuskDoll `inRecordSet` MementosDiscovered
      when theCreatureRecognizesYou do
        azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
        removeDoom attrs azathoth 1
      toDiscard GameSource attrs
      pure a
    _ -> TheFinalCountdown <$> liftRunMessage msg attrs
