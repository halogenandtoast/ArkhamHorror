module Arkham.Agenda.Cards.HumanityFading (humanityFading) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (InvestigatorDefeated)

newtype HumanityFading = HumanityFading AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

humanityFading :: AgendaCard HumanityFading
humanityFading = agenda (3, A) HumanityFading Cards.humanityFading (Static 7)

instance HasModifiersFor HumanityFading where
  getModifiersFor (HumanityFading attrs) =
    when (onSide A attrs) $ modifySelect attrs Anyone [HandSize (-4)]

instance RunMessage HumanityFading where
  runMessage msg a@(HumanityFading attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator $ investigatorDefeated attrs
      pure a
    _ -> HumanityFading <$> liftRunMessage msg attrs
