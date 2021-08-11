module Arkham.Types.Agenda.Cards.DeadOfNight where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype DeadOfNight = DeadOfNight AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadOfNight :: AgendaCard DeadOfNight
deadOfNight = agenda (2, A) DeadOfNight Cards.deadOfNight (Static 3)

instance HasActions DeadOfNight

instance HasModifiersFor env DeadOfNight where
  getModifiersFor _ (InvestigatorTarget _) (DeadOfNight a) =
    pure $ toModifiers a [HandSize (-3)]
  getModifiersFor _ _ _ = pure []

instance AgendaRunner env => RunMessage env DeadOfNight where
  runMessage msg a@(DeadOfNight attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      dormitoriesInPlay <- isJust <$> getLocationIdWithTitle "Dormitories"
      mExperimentId <- fmap unStoryEnemyId
        <$> getId (toCardCode Enemies.theExperiment)
      theExperiment <- EncounterCard <$> genEncounterCard Enemies.theExperiment
      scienceBuildingId <- fromJustNote "missing science building"
        <$> getLocationIdWithTitle "Science Building"
      a <$ pushAll
        ([ PlaceLocationMatching (LocationWithTitle "Dormitories")
         | not dormitoriesInPlay
         ]
        <> [ MoveToward (EnemyTarget eid) (LocationWithTitle "Dormitories")
           | eid <- maybeToList mExperimentId
           ]
        <> [ CreateEnemyAt theExperiment scienceBuildingId Nothing
           | isNothing mExperimentId
           ]
        <> [NextAgenda agendaId "02044"]
        )
    _ -> DeadOfNight <$> runMessage msg attrs
