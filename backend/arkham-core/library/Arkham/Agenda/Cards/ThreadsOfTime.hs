module Arkham.Agenda.Cards.ThreadsOfTime (
  ThreadsOfTime (..),
  threadsOfTime,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message hiding (InvestigatorEliminated)
import Arkham.Timing qualified as Timing

newtype ThreadsOfTime = ThreadsOfTime AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

threadsOfTime :: AgendaCard ThreadsOfTime
threadsOfTime = agenda (1, A) ThreadsOfTime Cards.threadsOfTime (Static 6)

instance HasAbilities ThreadsOfTime where
  getAbilities (ThreadsOfTime a) =
    let hasCard = HasCard (CardWithTitle "Relic of Ages")
     in [ mkAbility a 1
            $ ForcedAbility
            $ InvestigatorEliminated Timing.When
            $ AnyInvestigator
              [ HandWith hasCard
              , DiscardWith hasCard
              , DeckWith hasCard
              , HasMatchingAsset (AssetWithTitle "Relic of Ages")
              ]
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
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceToAgenda 1 Agendas.snappedThreads B (toSource attrs)
      pure a
    _ -> ThreadsOfTime <$> runMessage msg attrs
