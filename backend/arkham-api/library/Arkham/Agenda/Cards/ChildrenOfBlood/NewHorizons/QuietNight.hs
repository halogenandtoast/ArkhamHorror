module Arkham.Agenda.Cards.ChildrenOfBlood.NewHorizons.QuietNight (quietNight) where

import Arkham.Agenda.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype QuietNight = QuietNight AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quietNight :: AgendaCard QuietNight
quietNight = agenda (1, A) QuietNight Cards.quietNight (Static 6)

instance RunMessage QuietNight where
  runMessage msg a@(QuietNight attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> QuietNight <$> liftRunMessage msg attrs
