module Arkham.Types.Agenda.Cards.ChaosInTheCloverClub
  ( ChaosInTheCloverClub(..)
  , chaosInTheCloverClub
  )
where

import Arkham.Import

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Trait

newtype ChaosInTheCloverClub = ChaosInTheCloverClub Attrs
  deriving newtype (Show, ToJSON, FromJSON)

chaosInTheCloverClub :: ChaosInTheCloverClub
chaosInTheCloverClub = ChaosInTheCloverClub
  $ baseAttrs "02065" "Chaos in the Clover Club" (Agenda 3 A) (Static 7)

instance HasActions env ChaosInTheCloverClub where
  getActions i window (ChaosInTheCloverClub x) = getActions i window x

instance HasModifiersFor env ChaosInTheCloverClub where
  getModifiersFor = noModifiersFor

instance AgendaRunner env => RunMessage env ChaosInTheCloverClub where
  runMessage msg a@(ChaosInTheCloverClub attrs@Attrs {..}) = case msg of
    BeginEnemy | agendaSequence == Agenda 3 A -> do
      abominations <- getSetList @EnemyId Abomination
      abominationLocations <- traverse (getId @LocationId) abominations
      criminals <-
        concat <$> traverse (getSetList . ([Criminal], )) abominationLocations
      a <$ unshiftMessages [ Discard $ EnemyTarget eid | eid <- criminals ]
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 3 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      a <$ unshiftMessage
        (chooseOne leadInvestigatorId [Label "Continue" [Resolution 4]])
    _ -> ChaosInTheCloverClub <$> runMessage msg attrs
