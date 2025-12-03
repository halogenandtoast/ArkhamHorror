module Arkham.Location.Cards.PrivateRoom (privateRoom) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Helpers.Cost (getCanAffordCost, payEffectCost)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillTest.Type
import Arkham.SkillType

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
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure l
    PassedSkillTest iid _ (isAbilitySource attrs 1 -> True) Initiator {} (SkillSkillTest sType) _ -> do
      case sType of
        SkillWillpower -> do
          sid <- getRandom
          parley sid iid (attrs.ability 1) iid #intellect (Fixed 2)
        SkillIntellect -> do
          let cost = GroupClueCost (PerPlayer 1) (LocationWithId $ toId attrs)
          canAfford <- getCanAffordCost iid (attrs.ability 1) [] [] cost
          when canAfford $ do
            investigators <- select $ investigatorAt (toId attrs)
            randolph <- getSetAsideCard Assets.randolphCarterChainedToTheWakingWorld
            chooseOneM iid do
              labeled "Pay 1{perPlayer} clues" do
                payEffectCost iid attrs cost
                chooseOrRunOneM iid do
                  targets investigators (`takeControlOfSetAsideAsset` randolph)

              labeled "Do not pay" nothing
        _ -> error "invalid skill type"
      pure l
    _ -> PrivateRoom <$> liftRunMessage msg attrs
