module Arkham.Asset.Assets.GrannyOrne (grannyOrne) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GrannyOrne = GrannyOrne AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grannyOrne :: AssetCard GrannyOrne
grannyOrne = ally GrannyOrne Cards.grannyOrne (1, 3)

instance HasModifiersFor GrannyOrne where
  getModifiersFor (GrannyOrne a) = controllerGets a [SkillModifier #willpower 1]

instance HasAbilities GrannyOrne where
  getAbilities (GrannyOrne a) =
    [ restricted a 1 ControlsThis
        $ triggered
          (WouldHaveSkillTestResult #when (affectsOthers $ colocatedWithMatch You) #any #failure)
          (exhaust a)
    ]

instance RunMessage GrannyOrne where
  runMessage msg a@(GrannyOrne attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        chooseOneM iid do
          (cardI18n $ labeled' "grannyOrne.failBy1Less") do
            skillTestModifier sid (attrs.ability 1) sid (SkillTestResultValueModifier (-1))
            push RecalculateSkillTestResults
          (cardI18n $ labeled' "grannyOrne.failBy1More") do
            skillTestModifier sid (attrs.ability 1) sid (SkillTestResultValueModifier 1)
            push RecalculateSkillTestResults
      pure a
    _ -> GrannyOrne <$> liftRunMessage msg attrs
