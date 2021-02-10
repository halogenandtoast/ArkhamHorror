module Arkham.Types.Agenda.Cards.DeadOfNight where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


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
