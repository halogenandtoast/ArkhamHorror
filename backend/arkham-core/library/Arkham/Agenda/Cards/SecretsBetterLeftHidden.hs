module Arkham.Agenda.Cards.SecretsBetterLeftHidden (
  SecretsBetterLeftHidden (..),
  secretsBetterLeftHidden,
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

newtype SecretsBetterLeftHidden = SecretsBetterLeftHidden AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

secretsBetterLeftHidden :: AgendaCard SecretsBetterLeftHidden
secretsBetterLeftHidden =
  agenda
    (3, A)
    SecretsBetterLeftHidden
    Cards.secretsBetterLeftHidden
    (PerPlayer 3)

instance HasModifiersFor SecretsBetterLeftHidden where
  getModifiersFor (PhaseTarget MythosPhase) (SecretsBetterLeftHidden attrs) =
    pure $ toModifiers attrs [SkipMythosPhaseStep PlaceDoomOnAgendaStep]
  getModifiersFor _ _ = pure []

instance HasAbilities SecretsBetterLeftHidden where
  getAbilities (SecretsBetterLeftHidden attrs) =
    [ mkAbility attrs 1
        $ ForcedAbility
        $ PlacedCounterOnEnemy #after AnyEnemy AnySource #clue AnyValue
    ]

instance RunMessage SecretsBetterLeftHidden where
  runMessage msg a@(SecretsBetterLeftHidden attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      push R4
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 [(windowType -> Window.PlacedClues _ target n)] _ -> do
      push $ FlipClues target n
      pure a
    _ -> SecretsBetterLeftHidden <$> runMessage msg attrs
