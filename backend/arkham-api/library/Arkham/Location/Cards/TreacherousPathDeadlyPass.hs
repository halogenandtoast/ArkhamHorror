module Arkham.Location.Cards.TreacherousPathDeadlyPass (treacherousPathDeadlyPass) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (increaseThisFloodLevelOrElse)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TreacherousPathDeadlyPass = TreacherousPathDeadlyPass LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousPathDeadlyPass :: LocationCard TreacherousPathDeadlyPass
treacherousPathDeadlyPass = location TreacherousPathDeadlyPass Cards.treacherousPathDeadlyPass 0 (Static 1)

instance HasAbilities TreacherousPathDeadlyPass where
  getAbilities (TreacherousPathDeadlyPass a) =
    extendRevealed1 a $ skillTestAbility $ forcedAbility a 1 $ Enters #after You (be a)

instance RunMessage TreacherousPathDeadlyPass where
  runMessage msg l@(TreacherousPathDeadlyPass attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let level = maybe 1 (+ 1) attrs.row
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed level)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      increaseThisFloodLevelOrElse attrs
        $ chooseAndDiscardAssetMatching iid (attrs.ability 1) #ally
      pure l
    _ -> TreacherousPathDeadlyPass <$> liftRunMessage msg attrs
