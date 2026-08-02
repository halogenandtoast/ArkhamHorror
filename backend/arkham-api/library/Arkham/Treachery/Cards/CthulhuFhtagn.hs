module Arkham.Treachery.Cards.CthulhuFhtagn (cthulhuFhtagn) where

import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CthulhuFhtagn = CthulhuFhtagn TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cthulhuFhtagn :: TreacheryCard CthulhuFhtagn
cthulhuFhtagn = treachery CthulhuFhtagn Cards.cthulhuFhtagn

instance RunMessage CthulhuFhtagn where
  runMessage msg t@(CthulhuFhtagn attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #combat (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      -- "Take 2 horror or deal 1 direct damage to an investigator at your
      -- location" — you are at your own location, so you are a legal target too.
      investigators <- select $ investigatorAt iid
      chooseOneM iid $ withI18n do
        countVar 2 $ labeled' "takeHorror" $ assignHorror iid attrs 2
        countVar 1
          $ labeled' "dealDirectDamage"
          $ chooseTargetM iid investigators \iid' -> directDamage iid' attrs 1
      pure t
    _ -> CthulhuFhtagn <$> liftRunMessage msg attrs
