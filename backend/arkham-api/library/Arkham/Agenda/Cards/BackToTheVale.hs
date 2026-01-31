module Arkham.Agenda.Cards.BackToTheVale (backToTheVale) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype BackToTheVale = BackToTheVale AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backToTheVale :: AgendaCard BackToTheVale
backToTheVale = agenda (2, A) BackToTheVale Cards.backToTheVale (Static 12)

instance RunMessage BackToTheVale where
  runMessage msg a@(BackToTheVale attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> BackToTheVale <$> liftRunMessage msg attrs
