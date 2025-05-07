module Arkham.Agenda.Cards.TorturousDescent (torturousDescent) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Choose
import Arkham.Helpers.Query
import Arkham.Scenario.Deck

newtype TorturousDescent = TorturousDescent AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

torturousDescent :: AgendaCard TorturousDescent
torturousDescent = agenda (2, A) TorturousDescent Cards.torturousDescent (Static 7)

instance RunMessage TorturousDescent where
  runMessage msg a@(TorturousDescent attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      randomlyChooseFrom attrs lead MonstersDeck 1
      spawnConstanceDumaine <- not <$> slain Enemies.constanceDumaine
      when spawnConstanceDumaine $ createEnemyAtLocationMatching_ Enemies.constanceDumaine "Garden"
      advanceAgendaDeck attrs
      pure a
    ChoseCards _ chose | isTarget attrs chose.target -> do
      shuffleCardsIntoDeck Deck.EncounterDeck chose.cards
      pure a
    _ -> TorturousDescent <$> liftRunMessage msg attrs
