module Arkham.Agenda.Cards.ChildrenOfBlood.RiverOfBlood.TheFinalNight (theFinalNight) where

import Arkham.Agenda.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheFinalNight = TheFinalNight AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFinalNight :: AgendaCard TheFinalNight
theFinalNight = agenda (4, A) TheFinalNight Cards.theFinalNight (Static 5)

instance RunMessage TheFinalNight where
  runMessage msg a@(TheFinalNight attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheFinalNight <$> liftRunMessage msg attrs
