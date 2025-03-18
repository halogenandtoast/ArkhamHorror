module Arkham.Asset.Assets.GrannyOrne3 (grannyOrne3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.Helpers.SkillTest (getSkillTest)
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
    [ restricted a 1 ControlsThis
        $ triggered
          (WouldHaveSkillTestResult #when (affectsOthers $ at_ YourLocation) #any #failure)
          (exhaust a)
    ]

instance RunMessage GrannyOrne3 where
  runMessage msg a@(GrannyOrne3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      getSkillTest >>= traverse_ \st -> do
        chooseOneM iid do
          labeled "Get +1 skill value" do
            skillTestModifier st.id (attrs.ability 1) st.investigator (AnySkillValue 1)
            push RerunSkillTest
          labeled "Get -1 skill value" do
            skillTestModifier st.id (attrs.ability 1) st.investigator (AnySkillValue (-1))
            push RerunSkillTest
      pure a
    _ -> GrannyOrne3 <$> liftRunMessage msg attrs
