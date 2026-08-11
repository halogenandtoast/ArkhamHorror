module Arkham.Homebrew.DarkMatter.Assets.ThePallidMask (thePallidMask) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype ThePallidMask = ThePallidMask AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePallidMask :: AssetCard ThePallidMask
thePallidMask = asset ThePallidMask Cards.thePallidMask

{- | "[free] Exhaust The Pallid Mask: Test [intellect] (3). If you succeed, you may
move to any revealed location. If you fail, take 1 direct horror."
-}
instance HasAbilities ThePallidMask where
  getAbilities (ThePallidMask a) =
    [controlled a 1 ControlsThis $ FastAbility (exhaust a)]

instance RunMessage ThePallidMask where
  runMessage msg a@(ThePallidMask attrs) = runQueueT $ case msg of
    -- "Revelation - Put this card into play under the control of an investigator
    -- at your location."
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select $ colocatedWith iid
      chooseTargetM iid investigators \bearer -> putCardIntoPlay bearer attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      locations <- select RevealedLocation
      chooseOneM iid do
        labeled "$label.doNotMove" nothing
        targets locations $ moveTo (attrs.ability 1) iid
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      directHorror iid (attrs.ability 1) 1
      pure a
    _ -> ThePallidMask <$> liftRunMessage msg attrs
