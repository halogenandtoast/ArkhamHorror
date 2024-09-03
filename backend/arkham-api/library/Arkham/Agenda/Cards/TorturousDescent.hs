module Arkham.Agenda.Cards.TorturousDescent (TorturousDescent (..), torturousDescent) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Choose
import Arkham.Prelude
import Arkham.Scenario.Deck

newtype TorturousDescent = TorturousDescent AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

torturousDescent :: AgendaCard TorturousDescent
torturousDescent = agenda (2, A) TorturousDescent Cards.torturousDescent (Static 7)

instance RunMessage TorturousDescent where
  runMessage msg a@(TorturousDescent attrs) = case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead

      spawnConstanceDumaineMessages <- do
        spawnConstanceDumaine <- not <$> slain Enemies.constanceDumaine
        card <- genCard Enemies.constanceDumaine
        createConstanceDumaine <- createEnemyAtLocationMatching_ card "Garden"
        pure [createConstanceDumaine | spawnConstanceDumaine]

      pushAll
        $ [randomlyChooseFrom attrs lead MonstersDeck 1]
        <> spawnConstanceDumaineMessages
        <> [advanceAgendaDeck attrs]
      pure a
    ChoseCards _ chose | isTarget attrs chose.target -> do
      push $ ShuffleCardsIntoDeck Deck.EncounterDeck chose.cards
      pure a
    _ -> TorturousDescent <$> runMessage msg attrs
