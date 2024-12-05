module Arkham.Asset.Assets.GrannyOrne3 (grannyOrne3, GrannyOrne3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GrannyOrne3 = GrannyOrne3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grannyOrne3 :: AssetCard GrannyOrne3
grannyOrne3 = ally GrannyOrne3 Cards.grannyOrne3 (1, 3)

instance HasModifiersFor GrannyOrne3 where
  getModifiersFor (GrannyOrne3 a) = controllerGets a [SkillModifier #willpower 1, SkillModifier #intellect 1]

instance HasAbilities GrannyOrne3 where
  getAbilities (GrannyOrne3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (WouldHaveSkillTestResult #when (affectsOthers $ InvestigatorAt YourLocation) #any #failure)
          (exhaust a)
    ]

instance RunMessage GrannyOrne3 where
  runMessage msg a@(GrannyOrne3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        chooseOneM iid do
          labeled "Get +1 skill value" do
            skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
            push RerunSkillTest
          labeled "Get -1 skill value" do
            skillTestModifier sid (attrs.ability 1) iid (AnySkillValue (-1))
            push RerunSkillTest
      pure a
    _ -> GrannyOrne3 <$> liftRunMessage msg attrs
