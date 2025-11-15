module Arkham.Treachery.Cards.HuntersHungerB (huntersHungerB) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HuntersHungerB = HuntersHungerB TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntersHungerB :: TreacheryCard HuntersHungerB
huntersHungerB = treachery HuntersHungerB Cards.huntersHungerB

instance RunMessage HuntersHungerB where
  runMessage msg t@(HuntersHungerB attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      allies <- select $ assetControlledBy iid <> #ally <> AssetCanLeavePlayByNormalMeans
      others <- select $ not_ (InvestigatorWithId iid)
      sid <- getRandom
      chooseOneM iid $ scenarioI18n do
        labeledValidate' (notNull allies || notNull others) "huntersHunger.discard" do
          chooseTargetM iid allies $ toDiscardBy iid attrs
          for_ others \iid' -> assignHorror iid' attrs 1
        labeled' "huntersHunger.test" $ revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      directDamageAndHorror iid attrs 1 1
      pure t
    _ -> HuntersHungerB <$> liftRunMessage msg attrs
