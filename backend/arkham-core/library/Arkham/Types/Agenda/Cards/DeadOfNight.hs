{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.DeadOfNight where

import Arkham.Import

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.LocationMatcher

newtype DeadOfNight = DeadOfNight Attrs
  deriving newtype (Show, ToJSON, FromJSON)

deadOfNight :: DeadOfNight
deadOfNight =
  DeadOfNight $ baseAttrs "02043" 2 "Dead of Night" "Agenda 2a" (Static 3)

instance HasActions env DeadOfNight where
  getActions i window (DeadOfNight x) = getActions i window x

instance HasModifiersFor env DeadOfNight where
  getModifiersFor _ (InvestigatorTarget _) (DeadOfNight _) =
    pure [HandSize (-3)]
  getModifiersFor _ _ _ = pure []

instance AgendaRunner env => RunMessage env DeadOfNight where
  runMessage msg (DeadOfNight attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 2a" -> do
      dormitoriesInPlay <- elem (LocationName "Dormitories") <$> getList ()
      mExperimentId <- fmap unStoryEnemyId <$> getId (CardCode "02058")
      scienceBuildingId <- fromJustNote "missing science building"
        <$> getId (LocationName "Science Building")
      unshiftMessages
        $ [ PlaceLocationNamed "Dormitories" | not dormitoriesInPlay ]
        <> [ MoveToward
               (EnemyTarget eid)
               (LocationNamed $ LocationName "Dormitories")
           | eid <- maybeToList mExperimentId
           ]
        <> [ CreateEnemyAt "02058" scienceBuildingId | isNothing mExperimentId ]
        <> [NextAgenda agendaId "02044"]
      pure . DeadOfNight $ attrs & sequenceL .~ "Agenda 2b" & flippedL .~ True
    _ -> DeadOfNight <$> runMessage msg attrs
