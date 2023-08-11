module Arkham.Agenda.Cards.ChaosInTheCloverClub (
  ChaosInTheCloverClub (..),
  chaosInTheCloverClub,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Resolution
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype ChaosInTheCloverClub = ChaosInTheCloverClub AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosInTheCloverClub :: AgendaCard ChaosInTheCloverClub
chaosInTheCloverClub =
  agenda (3, A) ChaosInTheCloverClub Cards.chaosInTheCloverClub (Static 7)

instance HasAbilities ChaosInTheCloverClub where
  getAbilities (ChaosInTheCloverClub x) =
    [ mkAbility x 1 $
      ForcedAbility $
        PhaseBegins Timing.When $
          PhaseIs
            EnemyPhase
    | onSide A x
    ]

instance RunMessage ChaosInTheCloverClub where
  runMessage msg a@(ChaosInTheCloverClub attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      criminals <-
        selectList $ EnemyWithTrait Criminal <> EnemyAt (LocationWithEnemy $ EnemyWithTrait Abomination)
      pushAll [Discard (toSource attrs) $ EnemyTarget eid | eid <- criminals]
      pure a
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push $ chooseOne leadInvestigatorId [Label "Continue" [ScenarioResolution $ Resolution 4]]
      pure a
    _ -> ChaosInTheCloverClub <$> runMessage msg attrs
