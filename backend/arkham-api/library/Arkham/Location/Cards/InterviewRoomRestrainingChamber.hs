module Arkham.Location.Cards.InterviewRoomRestrainingChamber (interviewRoomRestrainingChamber) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype InterviewRoomRestrainingChamber = InterviewRoomRestrainingChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interviewRoomRestrainingChamber :: LocationCard InterviewRoomRestrainingChamber
interviewRoomRestrainingChamber =
  location InterviewRoomRestrainingChamber Cards.interviewRoomRestrainingChamber 4 (PerPlayer 1)

instance HasAbilities InterviewRoomRestrainingChamber where
  getAbilities (InterviewRoomRestrainingChamber a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here parleyAction_

instance RunMessage InterviewRoomRestrainingChamber where
  runMessage msg l@(InterviewRoomRestrainingChamber attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        skillLabeled #intellect $ parley sid iid (attrs.ability 1) iid #intellect (Fixed 4)
        skillLabeled #combat $ parley sid iid (attrs.ability 1) iid #combat (Fixed 4)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember InterviewedASubject
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid attrs 2
      pure l
    _ -> InterviewRoomRestrainingChamber <$> liftRunMessage msg attrs
