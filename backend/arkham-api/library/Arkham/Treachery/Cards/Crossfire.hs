module Arkham.Treachery.Cards.Crossfire (crossfire) where

import Arkham.Helpers.SkillTest.Lifted (beginSkillTest)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (beginSkillTest)

newtype Crossfire = Crossfire TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crossfire :: TreacheryCard Crossfire
crossfire = treachery Crossfire Cards.crossfire

instance RunMessage Crossfire where
  runMessage msg t@(Crossfire attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      beginSkillTest sid iid (toSource attrs) iid #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      investigators <- select $ InvestigatorAt (locationWithInvestigator iid)
      for_ investigators $ \iid' -> assignDamage iid' attrs 1
      pure t
    _ -> Crossfire <$> liftRunMessage msg attrs
