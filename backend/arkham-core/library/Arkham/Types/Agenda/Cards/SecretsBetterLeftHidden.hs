module Arkham.Types.Agenda.Cards.SecretsBetterLeftHidden
  ( SecretsBetterLeftHidden
  , secretsBetterLeftHidden
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Phase
import Arkham.Types.Resolution
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

newtype SecretsBetterLeftHidden = SecretsBetterLeftHidden AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsBetterLeftHidden :: AgendaCard SecretsBetterLeftHidden
secretsBetterLeftHidden = agenda
  (3, A)
  SecretsBetterLeftHidden
  Cards.secretsBetterLeftHidden
  (PerPlayer 3)

instance HasModifiersFor env SecretsBetterLeftHidden where
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

instance AgendaRunner env => RunMessage env SecretsBetterLeftHidden where
  runMessage msg a@(SecretsBetterLeftHidden attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [ScenarioResolution $ Resolution 4]
    UseCardAbility _ source [Window _ (Window.PlacedClues target n)] 1 _
      | isSource attrs source -> a <$ pushAll [FlipClues target n]
    _ -> SecretsBetterLeftHidden <$> runMessage msg attrs
