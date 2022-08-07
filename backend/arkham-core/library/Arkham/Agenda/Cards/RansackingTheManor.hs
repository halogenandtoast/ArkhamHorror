module Arkham.Agenda.Cards.RansackingTheManor
  ( RansackingTheManor(..)
  , ransackingTheManor
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Agenda.Types
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
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
      possessedOathspeaker <- getSetAsideCard Enemies.possessedOathspeaker
      spawnSebastienMoreau <-
        notElem (Recorded $ toCardCode Enemies.sebastienMoreau)
          <$> getRecordSet VIPsSlain
      spawnSebastienMoreauMessages <- if spawnSebastienMoreau
        then do
          card <- genCard Enemies.sebastienMoreau
          pure
            [ CreateEnemyAtLocationMatching
                card
                (LocationWithTitle "Entry Hall")
            ]
        else pure []

      a <$ pushAll
        ([ CreateEnemyAtLocationMatching
             possessedOathspeaker
             (LocationWithTitle "Entry Hall")
         ]
        <> spawnSebastienMoreauMessages
        <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
        )
    UseCardAbility _ source [Window _ (Window.PlacedClues target n)] 1 _
      | isSource attrs source -> a <$ pushAll [FlipClues target n]
    _ -> RansackingTheManor <$> runMessage msg attrs
