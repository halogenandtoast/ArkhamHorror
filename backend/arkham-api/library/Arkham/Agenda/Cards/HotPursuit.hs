module Arkham.Agenda.Cards.HotPursuit (HotPursuit (..), hotPursuit) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher

newtype HotPursuit = HotPursuit AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotPursuit :: AgendaCard HotPursuit
hotPursuit = agenda (2, A) HotPursuit Cards.hotPursuit (Static 9)

instance HasModifiersFor HotPursuit where
  getModifiersFor (HotPursuit a) =
    modifySelect a (not_ $ InVehicleMatching AnyAsset) [AdditionalActionCostOf #move 2]

instance RunMessage HotPursuit where
  runMessage msg a@(HotPursuit attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> HotPursuit <$> liftRunMessage msg attrs
