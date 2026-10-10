module Arkham.Homebrew.AgesUnwound.Agendas.FallingApart (fallingApart) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers (tumbleOutOfTime)

newtype FallingApart = FallingApart AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Each [[Adrift]] location is connected to the locations clockwise and
counter-clockwise from it."

All four Unstuck agendas print that line and an agenda is always in play, so the
ring's connections live on the locations themselves
('Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers.ringConnections') rather
than as a modifier each agenda has to grant and re-grant.
-}
fallingApart :: AgendaCard FallingApart
fallingApart = agenda (1, A) FallingApart Cards.fallingApart (Static 3)

instance RunMessage FallingApart where
  runMessage msg a@(FallingApart attrs) = runQueueT $ case msg of
    -- "Falling Still."
    AdvanceAgenda (isSide B attrs -> True) -> do
      tumbleOutOfTime attrs
      advanceAgendaDeck attrs
      pure a
    _ -> FallingApart <$> liftRunMessage msg attrs
