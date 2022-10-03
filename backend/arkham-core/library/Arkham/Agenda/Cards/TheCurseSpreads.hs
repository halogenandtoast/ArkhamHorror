module Arkham.Agenda.Cards.TheCurseSpreads
  ( TheCurseSpreads(..)
  , theCurseSpreads
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Agenda.Types
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Resolution
import Arkham.Timing qualified as Timing

newtype TheCurseSpreads = TheCurseSpreads AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCurseSpreads :: AgendaCard TheCurseSpreads
theCurseSpreads =
  agenda (3, A) TheCurseSpreads Cards.theCurseSpreads (Static 8)

instance HasAbilities TheCurseSpreads where
  getAbilities (TheCurseSpreads x) =
    [ mkAbility x 1 $ ForcedAbility $ PhaseEnds Timing.When $ PhaseIs
        InvestigationPhase
    ]

instance RunMessage TheCurseSpreads where
  runMessage msg a@(TheCurseSpreads attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      notEngaged <- isJust
        <$> selectOne (UnengagedEnemy <> enemyIs Cards.theRougarou)
      a <$ when notEngaged (push (PlaceDoom (toTarget attrs) 1))
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    _ -> TheCurseSpreads <$> runMessage msg attrs
