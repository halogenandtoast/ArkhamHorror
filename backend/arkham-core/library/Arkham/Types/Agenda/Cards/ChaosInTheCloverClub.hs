module Arkham.Types.Agenda.Cards.ChaosInTheCloverClub
  ( ChaosInTheCloverClub(..)
  , chaosInTheCloverClub
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Target
import Arkham.Types.Trait

newtype ChaosInTheCloverClub = ChaosInTheCloverClub AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosInTheCloverClub :: AgendaCard ChaosInTheCloverClub
chaosInTheCloverClub =
  agenda (3, A) ChaosInTheCloverClub Cards.chaosInTheCloverClub (Static 7)

instance HasActions ChaosInTheCloverClub
instance HasModifiersFor env ChaosInTheCloverClub

instance AgendaRunner env => RunMessage env ChaosInTheCloverClub where
  runMessage msg a@(ChaosInTheCloverClub attrs@AgendaAttrs {..}) = case msg of
    BeginEnemy | agendaSequence == Agenda 3 A -> do
      abominations <- getSetList @EnemyId Abomination
      abominationLocations <- traverse (getId @LocationId) abominations
      criminals <-
        concat <$> traverse (getSetList . ([Criminal], )) abominationLocations
      a <$ pushAll [ Discard $ EnemyTarget eid | eid <- criminals ]
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 3 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      a <$ push
        (chooseOne
          leadInvestigatorId
          [Label "Continue" [ScenarioResolution $ Resolution 4]]
        )
    _ -> ChaosInTheCloverClub <$> runMessage msg attrs
