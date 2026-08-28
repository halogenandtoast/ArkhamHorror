module Arkham.Treachery.Cards.MurderAtTheExcelsiorHotel.NoxiousFumes (noxiousFumes, NoxiousFumes (..)) where

import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.CardDefs.MurderAtTheExcelsiorHotel qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype NoxiousFumes = NoxiousFumes TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noxiousFumes :: TreacheryCard NoxiousFumes
noxiousFumes = treachery NoxiousFumes Cards.noxiousFumes

-- Which option each investigator took. The outcome cannot be recovered from the
-- test's SkillType, which ChangeSkillTestType (Money Talks) rewrites in place.
chosenSkills :: TreacheryAttrs -> [(InvestigatorId, SkillType)]
chosenSkills attrs = toResultDefault [] attrs.meta

recordChoice :: InvestigatorId -> SkillType -> TreacheryAttrs -> TreacheryAttrs
recordChoice iid sType attrs =
  setMeta ((iid, sType) : filter ((/= iid) . fst) (chosenSkills attrs)) attrs

choseSkill :: InvestigatorId -> SkillType -> TreacheryAttrs -> Bool
choseSkill iid sType attrs = lookup iid (chosenSkills attrs) == Just sType

instance RunMessage NoxiousFumes where
  runMessage msg t@(NoxiousFumes attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select $ colocatedWith iid
      for_ investigators (`forInvestigator` msg)
      doStep 1 msg
      pure . NoxiousFumes $ attrs & waitingL .~ True
    ForInvestigator iid (Revelation _ (isSource attrs -> True)) -> do
      chooseOneM iid do
        skillLabeled #agility $ doStep 2 msg
        skillLabeled #combat $ doStep 3 msg
      pure t
    DoStep 2 (ForInvestigator iid (Revelation _ (isSource attrs -> True))) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure . NoxiousFumes $ recordChoice iid #agility attrs
    DoStep 3 (ForInvestigator iid (Revelation _ (isSource attrs -> True))) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #combat (Fixed 3)
      pure . NoxiousFumes $ recordChoice iid #combat attrs
    DoStep 1 (Revelation _iid (isSource attrs -> True)) -> do
      pure . NoxiousFumes $ attrs & waitingL .~ False
    PassedSkillTest iid _ (isSource attrs -> True) Initiator {} _ _ | choseSkill iid #agility attrs -> do
      locations <- getConnectedMoveLocations iid attrs
      chooseTargetM iid locations $ moveTo attrs iid
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) Initiator {} _ _ | choseSkill iid #agility attrs -> do
      assignDamage iid attrs 2
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) Initiator {} _ n | choseSkill iid #combat attrs -> do
      assignDamage iid attrs n
      pure t
    _ -> NoxiousFumes <$> liftRunMessage msg attrs
