module Arkham.Location.Cards.InterviewRoomIchorFilledChamber (interviewRoomIchorFilledChamber) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype InterviewRoomIchorFilledChamber = InterviewRoomIchorFilledChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interviewRoomIchorFilledChamber :: LocationCard InterviewRoomIchorFilledChamber
interviewRoomIchorFilledChamber =
  location
    InterviewRoomIchorFilledChamber
    Cards.interviewRoomIchorFilledChamber
    1
    (PerPlayer 1)

instance HasAbilities InterviewRoomIchorFilledChamber where
  getAbilities (InterviewRoomIchorFilledChamber a) =
    extendRevealed1 a $ skillTestAbility $ mkAbility a 1 $ forced $ Enters #after You (be a)

instance RunMessage InterviewRoomIchorFilledChamber where
  runMessage msg l@(InterviewRoomIchorFilledChamber attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure l
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      assignHorror iid (attrs.ability 1) n
      pure l
    _ -> InterviewRoomIchorFilledChamber <$> liftRunMessage msg attrs
