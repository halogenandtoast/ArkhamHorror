module Arkham.Agenda.Cards.TheTruthIsHidden (
  TheTruthIsHidden (..),
  theTruthIsHidden,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Phase
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TheTruthIsHidden = TheTruthIsHidden AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theTruthIsHidden :: AgendaCard TheTruthIsHidden
theTruthIsHidden =
  agenda (1, A) TheTruthIsHidden Cards.theTruthIsHidden (PerPlayer 2)

instance HasModifiersFor TheTruthIsHidden where
  getModifiersFor (PhaseTarget MythosPhase) (TheTruthIsHidden attrs) =
    pure $ toModifiers attrs [SkipMythosPhaseStep PlaceDoomOnAgendaStep]
  getModifiersFor _ _ = pure []

instance HasAbilities TheTruthIsHidden where
  getAbilities (TheTruthIsHidden attrs) =
    [ mkAbility attrs 1 $ ForcedAbility $ PlacedCounterOnEnemy #after AnyEnemy AnySource #clue AnyValue
    ]

instance RunMessage TheTruthIsHidden where
  runMessage msg a@(TheTruthIsHidden attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      pushAll [ShuffleEncounterDiscardBackIn, advanceAgendaDeck attrs]
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 [(windowType -> Window.PlacedClues _ target n)] _ -> do
      push $ FlipClues target n
      pure a
    _ -> TheTruthIsHidden <$> runMessage msg attrs
