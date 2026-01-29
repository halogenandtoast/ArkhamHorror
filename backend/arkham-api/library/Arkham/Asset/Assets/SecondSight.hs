module Arkham.Asset.Assets.SecondSight (secondSight) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest.Lifted
import Arkham.I18n
import Arkham.Investigate.Types
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype SecondSight = SecondSight AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secondSight :: AssetCard SecondSight
secondSight = asset SecondSight Cards.secondSight

instance HasAbilities SecondSight where
  getAbilities (SecondSight a) =
    [skillTestAbility $ controlled_ a 1 $ investigateActionWith_ #willpower]

instance RunMessage SecondSight where
  runMessage msg a@(SecondSight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid #cultist attrs attrs $ doStep 1 msg
      investigateEdit_ sid iid (attrs.ability 1) \i -> i {investigateSkillType = #willpower}
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      if attrs.use #charge == 0
        then do
          assignHorror iid (attrs.ability 1) 1
          toDiscardBy iid (attrs.ability 1) attrs
        else removeTokens (attrs.ability 1) attrs Charge 1
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      when (attrs.use #charge > 0) do
        withSkillTest \sid ->
          chooseOneM iid do
            labeled "Spend 1 charge to discover 1 additional clue" do
              removeTokens (attrs.ability 1) attrs Charge 1
              skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
            withI18n skip_
      pure a
    _ -> SecondSight <$> liftRunMessage msg attrs
