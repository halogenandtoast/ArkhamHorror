module Arkham.Asset.Assets.Moxie3 (moxie3, Moxie3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets, modifySelf)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher

newtype Moxie3 = Moxie3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moxie3 :: AssetCard Moxie3
moxie3 = assetWith Moxie3 Cards.moxie3 $ (healthL ?~ 3) . (sanityL ?~ 1)

instance HasAbilities Moxie3 where
  getAbilities (Moxie3 x) =
    [ wantsSkillTest (YourSkillTest $ oneOf [#willpower, #agility])
        $ controlledAbility x 1 (DuringSkillTest AnySkillTest)
        $ FastAbility (ResourceCost 1)
    ]

instance HasModifiersFor Moxie3 where
  getModifiersFor (Moxie3 a) = do
    self <-
      modifySelf a [NonDirectHorrorMustBeAssignToThisFirst, NonDirectDamageMustBeAssignToThisFirst]
    controller <- controllerGets a [SkillModifier #willpower 1, SkillModifier #agility 1]
    pure $ self <> controller

instance RunMessage Moxie3 where
  runMessage msg a@(Moxie3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #willpower 1, SkillModifier #agility 1]
      pure a
    _ -> Moxie3 <$> liftRunMessage msg attrs
