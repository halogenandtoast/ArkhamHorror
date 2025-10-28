module Arkham.Agenda.Cards.ThreadsOfTime (threadsOfTime) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (InvestigatorEliminated)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype ThreadsOfTime = ThreadsOfTime AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

threadsOfTime :: AgendaCard ThreadsOfTime
threadsOfTime = agenda (1, A) ThreadsOfTime Cards.threadsOfTime (Static 6)

instance HasAbilities ThreadsOfTime where
  getAbilities (ThreadsOfTime a) =
    [ mkAbility a 1
        $ forced
        $ InvestigatorEliminated #when
        $ oneOf [HandWith hasRelic, DiscardWith hasRelic, DeckWith hasRelic, HasMatchingAsset "Relic of Ages"]
    ]
   where
    hasRelic = HasCard "Relic of Ages"

instance RunMessage ThreadsOfTime where
  runMessage msg a@(ThreadsOfTime attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      formlessSpawn <- getSetAsideCard Enemies.formlessSpawn
      nexus <- selectJust $ locationIs Locations.nexusOfNKai
      createEnemyAt_ formlessSpawn nexus
      addToVictory attrs
      advanceAgendaDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceToAgenda attrs Cards.snappedThreads B
      pure a
    _ -> ThreadsOfTime <$> liftRunMessage msg attrs
