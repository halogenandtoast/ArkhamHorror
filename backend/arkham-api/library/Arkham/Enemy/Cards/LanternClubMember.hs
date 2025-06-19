module Arkham.Enemy.Cards.LanternClubMember (lanternClubMember) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Cost
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheMidwinterGala.Helpers

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
        withI18n $ labeled' "evade" $ automaticallyEvadeEnemy iid attrs
        when (resources >= 2) do
          scenarioI18n $ labeled' "lanternClubMember.discard" do
            spendResources iid 2
            toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> LanternClubMember <$> liftRunMessage msg attrs
