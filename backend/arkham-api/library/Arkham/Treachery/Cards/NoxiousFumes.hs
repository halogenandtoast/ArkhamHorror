module Arkham.Treachery.Cards.NoxiousFumes (noxiousFumes) where

import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.SkillTest.Type
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype NoxiousFumes = NoxiousFumes TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noxiousFumes :: TreacheryCard NoxiousFumes
noxiousFumes = treachery NoxiousFumes Cards.noxiousFumes

instance RunMessage NoxiousFumes where
  runMessage msg t@(NoxiousFumes attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select $ colocatedWith iid
      for_ investigators (`forInvestigator` msg)
      doStep 1 msg
      pure . NoxiousFumes $ attrs & waitingL .~ True
    ForInvestigator iid (Revelation _ (isSource attrs -> True)) -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#agility, #combat] \skill -> do
          skillLabeled skill $ revelationSkillTest sid iid attrs skill (Fixed 3)
      pure t
    DoStep 1 (Revelation _iid (isSource attrs -> True)) -> do
      pure . NoxiousFumes $ attrs & waitingL .~ False
    PassedSkillTest iid _ (isSource attrs -> True) Initiator {} (SkillSkillTest SkillAgility) _ -> do
      accessibleLocations <- getAccessibleLocations iid attrs
      chooseTargetM iid accessibleLocations $ moveTo attrs iid
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) Initiator {} (SkillSkillTest SkillAgility) _ -> do
      assignDamage iid attrs 2
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) Initiator {} (SkillSkillTest SkillCombat) n -> do
      assignDamage iid attrs n
      pure t
    _ -> NoxiousFumes <$> liftRunMessage msg attrs
