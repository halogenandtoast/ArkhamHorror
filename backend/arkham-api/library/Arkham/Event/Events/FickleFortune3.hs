module Arkham.Event.Events.FickleFortune3 (fickleFortune3) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype FickleFortune3 = FickleFortune3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fickleFortune3 :: EventCard FickleFortune3
fickleFortune3 = event FickleFortune3 Cards.fickleFortune3

instance RunMessage FickleFortune3 where
  runMessage msg e@(FickleFortune3 attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasDoom <-
        selectAny $ AgendaWithDoom (atLeast 1) <> NotAgenda (AgendaWithModifier CannotRemoveDoomOnThis)
      chooseOneM iid $ cardI18n $ scope "fickleFortune" do
        labeled' "place"
          $ doStep 1 msg
        when hasDoom do
          labeled' "remove"
            $ doStep 2 msg
      pure e
    DoStep 1 (Revelation _iid (isSource attrs -> True)) -> do
      placeDoomOnAgenda 1
      eachInvestigator $ \iid' -> do
        healDamage iid' attrs 3
        healHorror iid' attrs 3
      pure e
    DoStep 2 (Revelation iid (isSource attrs -> True)) -> do
      hasDoom <- select $ AgendaWithDoom (atLeast 1)
      chooseOrRunOneM iid do
        for_ hasDoom \aid -> do
          targeting aid $ removeDoom attrs aid 1

      eachInvestigator $ \iid' -> do
        directDamage iid' attrs 1
        directHorror iid' attrs 1
      pure e
    _ -> FickleFortune3 <$> liftRunMessage msg attrs
