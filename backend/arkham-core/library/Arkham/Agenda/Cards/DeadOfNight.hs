module Arkham.Agenda.Cards.DeadOfNight
  ( DeadOfNight(..)
  , deadOfNight
  ) where

import Arkham.Prelude

import Arkham.Agenda.Types
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype DeadOfNight = DeadOfNight AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadOfNight :: AgendaCard DeadOfNight
deadOfNight = agenda (2, A) DeadOfNight Cards.deadOfNight (Static 3)

instance HasModifiersFor DeadOfNight where
  getModifiersFor _ (InvestigatorTarget _) (DeadOfNight a) =
    pure $ toModifiers a [HandSize (-3)]
  getModifiersFor _ _ _ = pure []

instance RunMessage DeadOfNight where
  runMessage msg a@(DeadOfNight attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      dormitoriesInPlay <- isJust
        <$> selectOne (LocationWithTitle "Dormitories")
      mExperimentId <- selectOne (enemyIs Enemies.theExperiment)
      theExperiment <- EncounterCard <$> genEncounterCard Enemies.theExperiment
      scienceBuildingId <- fromJustNote "missing science building"
        <$> selectOne (LocationWithTitle "Science Building")
      a <$ pushAll
        ([ PlaceLocationMatching (CardWithTitle "Dormitories")
         | not dormitoriesInPlay
         ]
        <> [ MoveToward (EnemyTarget eid) (LocationWithTitle "Dormitories")
           | eid <- maybeToList mExperimentId
           ]
        <> [ CreateEnemyAt theExperiment scienceBuildingId Nothing
           | isNothing mExperimentId
           ]
        <> [AdvanceAgendaDeck agendaDeckId (toSource attrs)]
        )
    _ -> DeadOfNight <$> runMessage msg attrs
