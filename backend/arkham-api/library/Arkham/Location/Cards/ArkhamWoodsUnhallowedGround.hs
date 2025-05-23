module Arkham.Location.Cards.ArkhamWoodsUnhallowedGround (arkhamWoodsUnhallowedGround) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (arkhamWoodsUnhallowedGround)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ArkhamWoodsUnhallowedGround = ArkhamWoodsUnhallowedGround LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsUnhallowedGround :: LocationCard ArkhamWoodsUnhallowedGround
arkhamWoodsUnhallowedGround =
  location ArkhamWoodsUnhallowedGround Cards.arkhamWoodsUnhallowedGround 4 (PerPlayer 1)

instance HasAbilities ArkhamWoodsUnhallowedGround where
  getAbilities (ArkhamWoodsUnhallowedGround x) =
    extendRevealed1 x $ skillTestAbility $ forcedAbility x 1 $ Enters #after You (be x)

instance RunMessage ArkhamWoodsUnhallowedGround where
  runMessage msg l@(ArkhamWoodsUnhallowedGround attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 4)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamageAndHorror iid attrs 1 1
      pure l
    _ -> ArkhamWoodsUnhallowedGround <$> liftRunMessage msg attrs
