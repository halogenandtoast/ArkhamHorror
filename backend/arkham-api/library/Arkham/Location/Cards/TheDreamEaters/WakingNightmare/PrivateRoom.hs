module Arkham.Location.Cards.TheDreamEaters.WakingNightmare.PrivateRoom (privateRoom) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Helpers.Cost (getCanAffordCost, payEffectCost)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.I18n
import Arkham.Location.CardDefs.TheDreamEaters.WakingNightmare qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheDreamEaters.WakingNightmare.Helpers

newtype PrivateRoom = PrivateRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

privateRoom :: LocationCard PrivateRoom
privateRoom = location PrivateRoom Cards.privateRoom 4 (Static 0)

instance HasAbilities PrivateRoom where
  getAbilities (PrivateRoom attrs) =
    extendRevealed1 attrs $ skillTestAbility $ restricted attrs 1 Here parleyAction_

instance RunMessage PrivateRoom where
  runMessage msg l@(PrivateRoom attrs) = runQueueT $ case msg of
    -- The two parleys are chained by a rider on each test's id; keying them off
    -- the test's SkillType would break under ChangeSkillTestType (Money Talks).
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      onSucceedByEffect sid AnyValue (attrs.ability 1) sid $ doStep 1 msg
      parley sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure l
    DoStep 1 msg'@(UseThisAbility iid (isSource attrs -> True) 1) -> do
      sid <- getRandom
      onSucceedByEffect sid AnyValue (attrs.ability 1) sid $ doStep 2 msg'
      parley sid iid (attrs.ability 1) iid #intellect (Fixed 2)
      pure l
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      let cost = GroupClueCost (PerPlayer 1) (LocationWithId $ toId attrs)
      canAfford <- getCanAffordCost iid (attrs.ability 1) [] [] cost
      when canAfford $ do
        investigators <- select $ investigatorAt (toId attrs)
        randolph <- getSetAsideCard Assets.randolphCarterChainedToTheWakingWorld
        chooseOneM iid $ scenarioI18n $ scope "privateRoom" do
          labeled' "payClues" do
            payEffectCost iid attrs cost
            chooseOrRunOneM iid do
              targets investigators (`takeControlOfSetAsideAsset` randolph)

          labeled' "doNotPay" nothing
      pure l
    _ -> PrivateRoom <$> liftRunMessage msg attrs
