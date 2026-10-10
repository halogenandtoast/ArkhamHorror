module Arkham.Homebrew.AgesUnwound.Agendas.Spring (spring) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (putTaskIntoPlay)

newtype Spring = Spring AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spring :: AgendaCard Spring
spring = agenda (3, A) Spring Cards.spring (Static 4)

-- | "If there is only 1 investigator in the game, this agenda gets +1 doom threshold."
instance HasModifiersFor Spring where
  getModifiersFor (Spring a) = do
    n <- getPlayerCount
    modifySelf a [DoomThresholdModifier 1 | n == 1]

instance RunMessage Spring where
  runMessage msg a@(Spring attrs) = runQueueT $ case msg of
    {- "__Time Runs Short__ - Put the set-aside Final Preparations treachery into
    play next to the act deck." -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      putTaskIntoPlay Treacheries.finalPreparations
      advanceAgendaDeck attrs
      pure a
    _ -> Spring <$> liftRunMessage msg attrs
