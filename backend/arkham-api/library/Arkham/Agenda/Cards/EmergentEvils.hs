module Arkham.Agenda.Cards.EmergentEvils (emergentEvils) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Scenarios.SmokeAndMirrors.Helpers

newtype EmergentEvils = EmergentEvils AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergentEvils :: AgendaCard EmergentEvils
emergentEvils = agenda (2, A) EmergentEvils Cards.emergentEvils (Static 8)

instance HasAbilities EmergentEvils where
  getAbilities (EmergentEvils a) =
    [scenarioI18n $ withI18nTooltip "emergentEvils.resign " $ mkAbility a 1 resignAction_ | onSide A a]

instance RunMessage EmergentEvils where
  runMessage msg a@(EmergentEvils attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator resign
      pure a
    _ -> EmergentEvils <$> liftRunMessage msg attrs
