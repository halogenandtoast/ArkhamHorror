module Arkham.Agenda.Cards.HotPursuit (hotPursuit) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype HotPursuit = HotPursuit AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotPursuit :: AgendaCard HotPursuit
hotPursuit = agenda (2, A) HotPursuit Cards.hotPursuit (Static 9)

instance HasModifiersFor HotPursuit where
  getModifiersFor (HotPursuit a) =
    modifySelect a (not_ $ InVehicleMatching AnyAsset) [AdditionalActionCostOf #move 2]

instance HasAbilities HotPursuit where
  getAbilities (HotPursuit a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage HotPursuit where
  runMessage msg a@(HotPursuit attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceRoad
      pure a
    _ -> HotPursuit <$> liftRunMessage msg attrs
