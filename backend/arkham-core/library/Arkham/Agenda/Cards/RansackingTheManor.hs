module Arkham.Agenda.Cards.RansackingTheManor
  ( RansackingTheManor(..)
  , ransackingTheManor
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype RansackingTheManor = RansackingTheManor AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ransackingTheManor :: AgendaCard RansackingTheManor
ransackingTheManor =
  agenda (2, A) RansackingTheManor Cards.ransackingTheManor (PerPlayer 2)

instance HasModifiersFor RansackingTheManor where
  getModifiersFor (PhaseTarget MythosPhase) (RansackingTheManor attrs) =
    pure $ toModifiers attrs [SkipMythosPhaseStep PlaceDoomOnAgendaStep]
  getModifiersFor _ _ = pure []

instance HasAbilities RansackingTheManor where
  getAbilities (RansackingTheManor attrs) =
    [ mkAbility attrs 1 $ ForcedAbility $ PlacedCounterOnEnemy
        Timing.After
        AnyEnemy
        ClueCounter
        AnyValue
    ]

instance RunMessage RansackingTheManor where
  runMessage msg a@(RansackingTheManor attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      spawnSebastienMoreau <- not <$> slain Enemies.sebastienMoreau
      spawnSebastienMoreauMessages <- do
        card <- genCard Enemies.sebastienMoreau
        createEnemyAtLocationMatching_ card (LocationWithTitle "Entry Hall")
      spawnPossessedOathspeaker <- do
        possessedOathspeaker <- getSetAsideCard Enemies.possessedOathspeaker
        createEnemyAtLocationMatching_
          possessedOathspeaker
          (LocationWithTitle "Entry Hall")

      pushAll
        $ spawnPossessedOathspeaker
        : [ spawnSebastienMoreauMessages | spawnSebastienMoreau ]
        <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      pure a
    UseCardAbility _ source 1 [Window _ (Window.PlacedClues target n)] _
      | isSource attrs source -> do
        pushAll [FlipClues target n]
        pure a
    _ -> RansackingTheManor <$> runMessage msg attrs
