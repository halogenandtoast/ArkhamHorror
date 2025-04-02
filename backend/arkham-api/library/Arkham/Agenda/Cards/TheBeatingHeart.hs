module Arkham.Agenda.Cards.TheBeatingHeart (theBeatingHeart) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Matcher

newtype TheBeatingHeart = TheBeatingHeart AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities TheBeatingHeart where
  getAbilities (TheBeatingHeart a) = [restricted a 1 ElectrostaticDetonation $ forced AnyWindow]

theBeatingHeart :: AgendaCard TheBeatingHeart
theBeatingHeart = agenda (1, A) TheBeatingHeart Cards.theBeatingHeart (Static 5)

instance RunMessage TheBeatingHeart where
  runMessage msg a@(TheBeatingHeart attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push R3
      pure a
    _ -> TheBeatingHeart <$> liftRunMessage msg attrs
