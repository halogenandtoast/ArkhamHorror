module Arkham.Types.Agenda.Cards.TheCurseSpreads
  ( TheCurseSpreads(..)
  , theCurseSpreads
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Phase
import Arkham.Types.Resolution
import Arkham.Types.Timing qualified as Timing

newtype TheCurseSpreads = TheCurseSpreads AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCurseSpreads :: AgendaCard TheCurseSpreads
theCurseSpreads =
  agenda (3, A) TheCurseSpreads Cards.theCurseSpreads (Static 8)

instance HasAbilities TheCurseSpreads where
  getAbilities (TheCurseSpreads x) =
    [ mkAbility x 1 $ ForcedAbility $ PhaseEnds Timing.When $ PhaseIs
        InvestigationPhase
    ]

instance AgendaRunner env => RunMessage env TheCurseSpreads where
  runMessage msg a@(TheCurseSpreads attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      notEngaged <- isJust
        <$> selectOne (UnengagedEnemy <> enemyIs Cards.theRougarou)
      a <$ when notEngaged (push (PlaceDoom (toTarget attrs) 1))
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    _ -> TheCurseSpreads <$> runMessage msg attrs
