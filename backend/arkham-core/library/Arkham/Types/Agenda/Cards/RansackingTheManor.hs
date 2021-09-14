module Arkham.Types.Agenda.Cards.RansackingTheManor
  ( RansackingTheManor
  , ransackingTheManor
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Types.Ability
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Phase
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

newtype RansackingTheManor = RansackingTheManor AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ransackingTheManor :: AgendaCard RansackingTheManor
ransackingTheManor =
  agenda (2, A) RansackingTheManor Cards.ransackingTheManor (PerPlayer 2)

instance HasModifiersFor env RansackingTheManor where
  getModifiersFor _ (PhaseTarget MythosPhase) (RansackingTheManor attrs) =
    pure $ toModifiers attrs [SkipMythosPhaseStep PlaceDoomOnAgendaStep]
  getModifiersFor _ _ _ = pure []

instance HasAbilities RansackingTheManor where
  getAbilities (RansackingTheManor attrs) =
    [ mkAbility attrs 1 $ ForcedAbility $ PlacedCounterOnEnemy
        Timing.After
        AnyEnemy
        ClueCounter
        AnyValue
    ]

instance AgendaRunner env => RunMessage env RansackingTheManor where
  runMessage msg a@(RansackingTheManor attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      possessedOathspeaker <- getSetAsideCard Enemies.possessedOathspeaker
      spawnSebastienMoreau <-
        elem (Recorded $ toCardCode Enemies.sebastienMoreau)
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
        <> [NextAgenda aid "03123"]
        )
    UseCardAbility _ source [Window _ (Window.PlacedClues target n)] 1 _
      | isSource attrs source -> a <$ pushAll [FlipClues target n]
    _ -> RansackingTheManor <$> runMessage msg attrs
