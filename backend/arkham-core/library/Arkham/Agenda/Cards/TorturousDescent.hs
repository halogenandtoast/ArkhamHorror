module Arkham.Agenda.Cards.TorturousDescent (
  TorturousDescent (..),
  torturousDescent,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Scenario.Deck

newtype TorturousDescent = TorturousDescent AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

torturousDescent :: AgendaCard TorturousDescent
torturousDescent =
  agenda (2, A) TorturousDescent Cards.torturousDescent (Static 7)

instance RunMessage TorturousDescent where
  runMessage msg a@(TorturousDescent attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      lead <- getLead

      spawnConstanceDumaineMessages <- do
        spawnConstanceDumaine <- not <$> slain Enemies.constanceDumaine
        card <- genCard Enemies.constanceDumaine
        createConstanceDumaine <-
          createEnemyAtLocationMatching_ card
            $ LocationWithTitle "Garden"
        pure [createConstanceDumaine | spawnConstanceDumaine]

      pushAll
        $ [DrawRandomFromScenarioDeck lead MonstersDeck (toTarget attrs) 1]
        <> spawnConstanceDumaineMessages
        <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      pure a
    DrewFromScenarioDeck _ _ (isTarget attrs -> True) cards -> do
      push $ ShuffleCardsIntoDeck Deck.EncounterDeck cards
      pure a
    _ -> TorturousDescent <$> runMessage msg attrs
