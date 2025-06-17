module Arkham.Enemy.Cards.LanternClubMember (lanternClubMember) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Cost
import Arkham.Message.Lifted.Choose

newtype LanternClubMember = LanternClubMember EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lanternClubMember :: EnemyCard LanternClubMember
lanternClubMember = enemy LanternClubMember Cards.lanternClubMember (3, Static 2, 2) (0, 1)

instance HasAbilities LanternClubMember where
  getAbilities (LanternClubMember a) =
    extend1 a $ restricted a 1 OnSameLocation $ parleyAction (ResourceCost 1)

instance RunMessage LanternClubMember where
  runMessage msg e@(LanternClubMember attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resources <- getSpendableResources iid
      chooseOrRunOneM iid do
        labeled "Evade" $ automaticallyEvadeEnemy iid attrs
        when (resources >= 2) do
          labeled "Spend 2 additional resources to discard" do
            spendResources iid 2
            toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> LanternClubMember <$> liftRunMessage msg attrs
