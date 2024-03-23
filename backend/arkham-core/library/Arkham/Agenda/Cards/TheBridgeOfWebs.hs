module Arkham.Agenda.Cards.TheBridgeOfWebs (
  TheBridgeOfWebs (..),
  theBridgeOfWebs,
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheBridgeOfWebs = TheBridgeOfWebs AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBridgeOfWebs :: AgendaCard TheBridgeOfWebs
theBridgeOfWebs = agenda (1, A) TheBridgeOfWebs Cards.theBridgeOfWebs (Static 7)

instance RunMessage TheBridgeOfWebs where
  runMessage msg a@(TheBridgeOfWebs attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheBridgeOfWebs <$> runMessage msg attrs
