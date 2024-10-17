module Arkham.Agenda.Cards.TerrorAtFalconPoint
  ( TerrorAtFalconPoint(..)
  , terrorAtFalconPoint
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TerrorAtFalconPoint = TerrorAtFalconPoint AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorAtFalconPoint :: AgendaCard TerrorAtFalconPoint
terrorAtFalconPoint = agenda (4, A) TerrorAtFalconPoint Cards.terrorAtFalconPoint (Static 12)

instance RunMessage TerrorAtFalconPoint where
  runMessage msg a@(TerrorAtFalconPoint attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TerrorAtFalconPoint <$> liftRunMessage msg attrs
