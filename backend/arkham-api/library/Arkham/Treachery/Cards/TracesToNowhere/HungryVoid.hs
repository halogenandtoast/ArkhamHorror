module Arkham.Treachery.Cards.TracesToNowhere.HungryVoid (hungryVoid) where

import Arkham.Helpers.Agenda
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Treachery.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HungryVoid = HungryVoid TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hungryVoid :: TreacheryCard HungryVoid
hungryVoid = treachery HungryVoid Cards.hungryVoid

instance RunMessage HungryVoid where
  runMessage msg t@(HungryVoid attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid $ scenarioI18n do
        labeled "hungryVoid.takeDamageAndHorror" $ assignDamageAndHorror iid attrs 1 1
        labeled "hungryVoid.placeDoom" do
          agenda <- getCurrentAgenda
          placeDoom attrs agenda 1
      pure t
    _ -> HungryVoid <$> liftRunMessage msg attrs
