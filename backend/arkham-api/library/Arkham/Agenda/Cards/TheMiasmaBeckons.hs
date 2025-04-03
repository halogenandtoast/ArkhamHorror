module Arkham.Agenda.Cards.TheMiasmaBeckons (theMiasmaBeckons) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher

newtype TheMiasmaBeckons = TheMiasmaBeckons AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMiasmaBeckons :: AgendaCard TheMiasmaBeckons
theMiasmaBeckons = agenda (2, A) TheMiasmaBeckons Cards.theMiasmaBeckons (Static 6)

instance HasAbilities TheMiasmaBeckons where
  getAbilities (TheMiasmaBeckons a) = [restricted a 1 ElectrostaticDetonation $ forced AnyWindow]

instance RunMessage TheMiasmaBeckons where
  runMessage msg a@(TheMiasmaBeckons attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectForMaybeM (InvestigatorWithSeal SealA) \iid -> do
        active <- iid <=~> InvestigatorWithActiveSeal SealA
        loseResources iid attrs $ if active then 6 else 3

      selectForMaybeM (InvestigatorWithSeal SealB) \iid -> do
        active <- iid <=~> InvestigatorWithActiveSeal SealB
        loseActions iid attrs $ if active then 2 else 1

      selectForMaybeM (InvestigatorWithSeal SealC) \iid -> do
        active <- iid <=~> InvestigatorWithActiveSeal SealC
        assignHorror iid attrs $ if active then 2 else 1

      selectForMaybeM (InvestigatorWithSeal SealD) \iid -> do
        active <- iid <=~> InvestigatorWithActiveSeal SealD
        assignDamage iid attrs $ if active then 2 else 1

      selectForMaybeM (InvestigatorWithSeal SealE) \iid -> do
        active <- iid <=~> InvestigatorWithActiveSeal SealE
        randomDiscardN iid attrs $ if active then 4 else 2

      advanceAgendaDeck attrs
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push R3
      pure a
    _ -> TheMiasmaBeckons <$> liftRunMessage msg attrs
