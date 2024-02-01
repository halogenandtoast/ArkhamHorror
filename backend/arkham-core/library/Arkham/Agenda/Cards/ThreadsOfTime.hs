module Arkham.Agenda.Cards.ThreadsOfTime (ThreadsOfTime (..), threadsOfTime) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude

newtype ThreadsOfTime = ThreadsOfTime AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

threadsOfTime :: AgendaCard ThreadsOfTime
threadsOfTime = agenda (1, A) ThreadsOfTime Cards.threadsOfTime (Static 6)

instance HasAbilities ThreadsOfTime where
  getAbilities (ThreadsOfTime a) =
    let hasRelic = HasCard "Relic of Ages"
     in [ mkAbility a 1
            $ ForcedAbility
            $ InvestigatorEliminated #when
            $ oneOf
              [HandWith hasRelic, DiscardWith hasRelic, DeckWith hasRelic, HasMatchingAsset "Relic of Ages"]
        ]

instance RunMessage ThreadsOfTime where
  runMessage msg a@(ThreadsOfTime attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      formlessSpawn <- getSetAsideCard Enemies.formlessSpawn
      nexus <- selectJust $ locationIs Locations.nexusOfNKai
      createFormlessSpawn <- createEnemyAt_ formlessSpawn nexus Nothing
      pushAll
        [ ShuffleEncounterDiscardBackIn
        , createFormlessSpawn
        , AddToVictory (toTarget attrs)
        , advanceAgendaDeck attrs
        ]
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AdvanceToAgenda 1 Agendas.snappedThreads B (toSource attrs)
      pure a
    _ -> ThreadsOfTime <$> runMessage msg attrs
