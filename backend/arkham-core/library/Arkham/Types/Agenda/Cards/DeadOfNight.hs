module Arkham.Types.Agenda.Cards.DeadOfNight where


import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner

newtype DeadOfNight = DeadOfNight AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadOfNight :: DeadOfNight
deadOfNight =
  DeadOfNight $ baseAttrs "02043" "Dead of Night" (Agenda 2 A) (Static 3)

instance HasActions env DeadOfNight where
  getActions i window (DeadOfNight x) = getActions i window x

instance HasModifiersFor env DeadOfNight where
  getModifiersFor _ (InvestigatorTarget _) (DeadOfNight a) =
    pure $ toModifiers a [HandSize (-3)]
  getModifiersFor _ _ _ = pure []

instance AgendaRunner env => RunMessage env DeadOfNight where
  runMessage msg a@(DeadOfNight attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      dormitoriesInPlay <- isJust <$> getLocationIdWithTitle "Dormitories"
      mExperimentId <- fmap unStoryEnemyId <$> getId (CardCode "02058")
      theExperiment <- EncounterCard <$> genEncounterCard "02058"
      scienceBuildingId <- fromJustNote "missing science building"
        <$> getLocationIdWithTitle "Science Building"
      a <$ unshiftMessages
        ([ PlaceLocationMatching (LocationWithTitle "Dormitories")
         | not dormitoriesInPlay
         ]
        <> [ MoveToward (EnemyTarget eid) (LocationWithTitle "Dormitories")
           | eid <- maybeToList mExperimentId
           ]
        <> [ CreateEnemyAt theExperiment scienceBuildingId
           | isNothing mExperimentId
           ]
        <> [NextAgenda agendaId "02044"]
        )
    _ -> DeadOfNight <$> runMessage msg attrs
