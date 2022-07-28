module Arkham.Agenda.Cards.SecretsBetterLeftHidden
  ( SecretsBetterLeftHidden(..)
  , secretsBetterLeftHidden
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Types
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Resolution
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype SecretsBetterLeftHidden = SecretsBetterLeftHidden AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsBetterLeftHidden :: AgendaCard SecretsBetterLeftHidden
secretsBetterLeftHidden = agenda
  (3, A)
  SecretsBetterLeftHidden
  Cards.secretsBetterLeftHidden
  (PerPlayer 3)

instance HasModifiersFor SecretsBetterLeftHidden where
  getModifiersFor _ (PhaseTarget MythosPhase) (SecretsBetterLeftHidden attrs) =
    pure $ toModifiers attrs [SkipMythosPhaseStep PlaceDoomOnAgendaStep]
  getModifiersFor _ _ _ = pure []

instance HasAbilities SecretsBetterLeftHidden where
  getAbilities (SecretsBetterLeftHidden attrs) =
    [ mkAbility attrs 1 $ ForcedAbility $ PlacedCounterOnEnemy
        Timing.After
        AnyEnemy
        ClueCounter
        AnyValue
    ]

instance RunMessage SecretsBetterLeftHidden where
  runMessage msg a@(SecretsBetterLeftHidden attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [ScenarioResolution $ Resolution 4]
    UseCardAbility _ source [Window _ (Window.PlacedClues target n)] 1 _
      | isSource attrs source -> a <$ pushAll [FlipClues target n]
    _ -> SecretsBetterLeftHidden <$> runMessage msg attrs
