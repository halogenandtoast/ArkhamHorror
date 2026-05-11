module Arkham.Treachery.Cards.Crossfire (crossfire) where

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
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      selectEach (InvestigatorAt $ locationWithInvestigator iid) \iid' -> assignDamage iid' attrs 1
      pure t
    _ -> Crossfire <$> liftRunMessage msg attrs
