module Arkham.Asset.Assets.RelicOfAgesRepossessThePast (relicOfAgesRepossessThePast) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Doom (targetsWithDoom)
import Arkham.Helpers.Modifiers (ModifierType (..), semaphore)
import Arkham.Message.Lifted.Choose

newtype RelicOfAgesRepossessThePast = RelicOfAgesRepossessThePast AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicOfAgesRepossessThePast :: AssetCard RelicOfAgesRepossessThePast
relicOfAgesRepossessThePast = asset RelicOfAgesRepossessThePast Cards.relicOfAgesRepossessThePast

instance HasAbilities RelicOfAgesRepossessThePast where
  getAbilities (RelicOfAgesRepossessThePast a) =
    [skillTestAbility $ controlled_ a 1 $ FastAbility $ exhaust a]

instance RunMessage RelicOfAgesRepossessThePast where
  runMessage msg a@(RelicOfAgesRepossessThePast attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#willpower, #intellect] (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      semaphore attrs do
        gameModifier attrs attrs Semaphore
        ts <- targetsWithDoom
        chooseTargetM iid ts $ removeDoomFrom (attrs.ability 1) 1
      pure a
    _ -> RelicOfAgesRepossessThePast <$> liftRunMessage msg attrs
