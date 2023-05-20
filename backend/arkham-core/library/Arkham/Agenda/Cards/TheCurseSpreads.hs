module Arkham.Agenda.Cards.TheCurseSpreads (
  TheCurseSpreads (..),
  theCurseSpreads,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Timing qualified as Timing

newtype TheCurseSpreads = TheCurseSpreads AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCurseSpreads :: AgendaCard TheCurseSpreads
theCurseSpreads =
  agenda (3, A) TheCurseSpreads Cards.theCurseSpreads (Static 8)

instance HasAbilities TheCurseSpreads where
  getAbilities (TheCurseSpreads x) =
    [ mkAbility x 1 $
        ForcedAbility $
          PhaseEnds Timing.When $
            PhaseIs
              InvestigationPhase
    ]

instance RunMessage TheCurseSpreads where
  runMessage msg a@(TheCurseSpreads attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      notEngaged <- selectAny $ UnengagedEnemy <> enemyIs Cards.theRougarou
      a <$ when notEngaged (push (PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1))
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      a <$ push (scenarioResolution 1)
    _ -> TheCurseSpreads <$> runMessage msg attrs
