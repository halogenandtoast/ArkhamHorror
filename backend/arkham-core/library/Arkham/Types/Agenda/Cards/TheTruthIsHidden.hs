module Arkham.Types.Agenda.Cards.TheTruthIsHidden
  ( TheTruthIsHidden
  , theTruthIsHidden
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
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
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

newtype TheTruthIsHidden = TheTruthIsHidden AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTruthIsHidden :: AgendaCard TheTruthIsHidden
theTruthIsHidden =
  agenda (1, A) TheTruthIsHidden Cards.theTruthIsHidden (PerPlayer 2)

instance HasModifiersFor env TheTruthIsHidden where
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

instance AgendaRunner env => RunMessage env TheTruthIsHidden where
  runMessage msg a@(TheTruthIsHidden attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> a <$ pushAll
      [ ShuffleEncounterDiscardBackIn
      , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
      ]
    UseCardAbility _ source [Window _ (Window.PlacedClues target n)] 1 _
      | isSource attrs source -> a <$ pushAll [FlipClues target n]
    _ -> TheTruthIsHidden <$> runMessage msg attrs
