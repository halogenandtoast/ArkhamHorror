module Arkham.Asset.Assets.ScientificTheory3 (scientificTheory3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ScientificTheory3 = ScientificTheory3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scientificTheory3 :: AssetCard ScientificTheory3
scientificTheory3 = assetWith ScientificTheory3 Cards.scientificTheory3 $ (healthL ?~ 1) . (sanityL ?~ 3)

instance HasAbilities ScientificTheory3 where
  getAbilities (ScientificTheory3 x) =
    [ wantsSkillTest (YourSkillTest $ oneOf [#intellect, #combat])
        $ controlled x 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance HasModifiersFor ScientificTheory3 where
  getModifiersFor (ScientificTheory3 a) = do
    modifySelf a [NonDirectHorrorMustBeAssignToThisFirst, NonDirectDamageMustBeAssignToThisFirst]
    controllerGets a [SkillModifier #intellect 1, SkillModifier #combat 1]

instance RunMessage ScientificTheory3 where
  runMessage msg a@(ScientificTheory3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        chooseSkillM iid [#intellect, #combat] \kind ->
          skillTestModifier sid (attrs.ability 1) iid (SkillModifier kind 1)
      pure a
    _ -> ScientificTheory3 <$> liftRunMessage msg attrs
