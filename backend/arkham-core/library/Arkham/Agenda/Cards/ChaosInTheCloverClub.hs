module Arkham.Agenda.Cards.ChaosInTheCloverClub
  ( ChaosInTheCloverClub(..)
  , chaosInTheCloverClub
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Attrs
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Query
import Arkham.Resolution
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype ChaosInTheCloverClub = ChaosInTheCloverClub AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosInTheCloverClub :: AgendaCard ChaosInTheCloverClub
chaosInTheCloverClub =
  agenda (3, A) ChaosInTheCloverClub Cards.chaosInTheCloverClub (Static 7)

instance HasAbilities ChaosInTheCloverClub where
  getAbilities (ChaosInTheCloverClub x) =
    [ mkAbility x 1 $ ForcedAbility $ PhaseBegins Timing.When $ PhaseIs
        EnemyPhase
    | onSide A x
    ]

instance AgendaRunner env => RunMessage env ChaosInTheCloverClub where
  runMessage msg a@(ChaosInTheCloverClub attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
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
