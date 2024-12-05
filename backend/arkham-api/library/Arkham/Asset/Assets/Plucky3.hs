module Arkham.Asset.Assets.Plucky3 (plucky3, Plucky3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets, modifySelf)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher

newtype Plucky3 = Plucky3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plucky3 :: AssetCard Plucky3
plucky3 = assetWith Plucky3 Cards.plucky3 $ (healthL ?~ 1) . (sanityL ?~ 3)

instance HasAbilities Plucky3 where
  getAbilities (Plucky3 x) =
    [ wantsSkillTest (YourSkillTest $ oneOf [#willpower, #intellect])
        $ controlledAbility x 1 (DuringSkillTest AnySkillTest)
        $ FastAbility (ResourceCost 1)
    ]

instance HasModifiersFor Plucky3 where
  getModifiersFor (Plucky3 a) = do
    self <-
      modifySelf a [NonDirectHorrorMustBeAssignToThisFirst, NonDirectDamageMustBeAssignToThisFirst]
    controller <- controllerGets a [SkillModifier #willpower 1, SkillModifier #intellect 1]
    pure $ self <> controller

instance RunMessage Plucky3 where
  runMessage msg a@(Plucky3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        skillTestModifiers
          sid
          (attrs.ability 1)
          iid
          [SkillModifier #willpower 1, SkillModifier #intellect 1]
      pure a
    _ -> Plucky3 <$> liftRunMessage msg attrs
