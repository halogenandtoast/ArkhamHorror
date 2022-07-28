module Arkham.Agenda.Cards.TheTruthIsHidden
  ( TheTruthIsHidden(..)
  , theTruthIsHidden
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
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype TheTruthIsHidden = TheTruthIsHidden AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTruthIsHidden :: AgendaCard TheTruthIsHidden
theTruthIsHidden =
  agenda (1, A) TheTruthIsHidden Cards.theTruthIsHidden (PerPlayer 2)

instance HasModifiersFor TheTruthIsHidden where
  getModifiersFor _ (PhaseTarget MythosPhase) (TheTruthIsHidden attrs) =
    pure $ toModifiers attrs [SkipMythosPhaseStep PlaceDoomOnAgendaStep]
  getModifiersFor _ _ _ = pure []

instance HasAbilities TheTruthIsHidden where
  getAbilities (TheTruthIsHidden attrs) =
    [ mkAbility attrs 1 $ ForcedAbility $ PlacedCounterOnEnemy
        Timing.After
        AnyEnemy
        ClueCounter
        AnyValue
    ]

instance RunMessage TheTruthIsHidden where
  runMessage msg a@(TheTruthIsHidden attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> a <$ pushAll
      [ ShuffleEncounterDiscardBackIn
      , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
      ]
    UseCardAbility _ source [Window _ (Window.PlacedClues target n)] 1 _
      | isSource attrs source -> a <$ pushAll [FlipClues target n]
    _ -> TheTruthIsHidden <$> runMessage msg attrs
