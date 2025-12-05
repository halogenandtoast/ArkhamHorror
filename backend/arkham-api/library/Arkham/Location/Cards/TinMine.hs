module Arkham.Location.Cards.TinMine (tinMine) where

import Arkham.Ability
import Arkham.Helpers.Cost
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.ScenarioLogKey
import Arkham.Scenarios.ShadesOfSuffering.Helpers

newtype TinMine = TinMine LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tinMine :: LocationCard TinMine
tinMine = symbolLabel $ location TinMine Cards.tinMine 3 (PerPlayer 2)

instance HasAbilities TinMine where
  getAbilities (TinMine a) =
    extendRevealed
      a
      [ groupLimit PerGame $ skillTestAbility $ restricted a 1 Here actionAbility
      , mkAbility a 2 $ forced $ SkillTestResult #after (You <> investigatorAt a) AnySkillTest #failure
      ]

instance RunMessage TinMine where
  runMessage msg l@(TinMine attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      let cost = GroupClueCost (PerPlayer 1) (be attrs)
      whenM (getCanAffordCost iid (attrs.ability 1) [] [] cost) do
        chooseOneM iid $ scenarioI18n do
          labeled' "tinMine.pay" do
            payEffectCost iid attrs cost
            skillTestModifier sid (attrs.ability 1) sid SkillTestAutomaticallySucceeds
          unscoped skip_
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 6)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      drawCards iid (attrs.ability 1) 2
      remember FoundHiddenBones
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignDamage iid (attrs.ability 2) 1
      pure l
    _ -> TinMine <$> liftRunMessage msg attrs
