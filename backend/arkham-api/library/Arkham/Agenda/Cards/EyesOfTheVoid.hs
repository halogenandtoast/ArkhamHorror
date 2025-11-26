module Arkham.Agenda.Cards.EyesOfTheVoid (eyesOfTheVoid) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype EyesOfTheVoid = EyesOfTheVoid AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyesOfTheVoid :: AgendaCard EyesOfTheVoid
eyesOfTheVoid = agenda (2, A) EyesOfTheVoid Cards.eyesOfTheVoid (Static 6)

instance RunMessage EyesOfTheVoid where
  runMessage msg a@(EyesOfTheVoid attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> EyesOfTheVoid <$> liftRunMessage msg attrs
