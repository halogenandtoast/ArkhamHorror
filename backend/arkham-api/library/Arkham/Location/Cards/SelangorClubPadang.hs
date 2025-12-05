module Arkham.Location.Cards.SelangorClubPadang (selangorClubPadang) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), semaphore)
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype SelangorClubPadang = SelangorClubPadang LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

selangorClubPadang :: LocationCard SelangorClubPadang
selangorClubPadang = symbolLabel $ location SelangorClubPadang Cards.selangorClubPadang 3 (PerPlayer 2)

instance HasAbilities SelangorClubPadang where
  getAbilities (SelangorClubPadang a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here parleyAction_

instance RunMessage SelangorClubPadang where
  runMessage msg l@(SelangorClubPadang attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #combat (Fixed 4)
      pure l
    FailedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      remember EmbarrassedTheConsulate
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      semaphore attrs do
        roundModifier (attrs.ability 1) attrs Semaphore
        gainResources iid (attrs.ability 1) 3
      pure l
    _ -> SelangorClubPadang <$> liftRunMessage msg attrs
