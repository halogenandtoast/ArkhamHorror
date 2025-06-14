module Arkham.Enemy.Cards.LanternClubMember (
  lanternClubMember,
  LanternClubMember(..),
) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype LanternClubMember = LanternClubMember EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lanternClubMember :: EnemyCard LanternClubMember
lanternClubMember = enemy LanternClubMember Cards.lanternClubMember (3, Static 2, 2) (0, 1)

instance HasAbilities LanternClubMember where
  getAbilities (LanternClubMember a) = withBaseAbilities a [
      restrictedAbility a 1 OnSameLocation
        $ ActionAbility [#parley] (ActionCost 1 <> ResourceCost 1)
    ]

instance HasModifiersFor LanternClubMember where
  getModifiersFor _ = pure []

instance RunMessage LanternClubMember where
  runMessage msg e@(LanternClubMember attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      resources <- getSpendableResources iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          ([Label "Evade" [EnemyEvaded iid attrs.id]]
            <> [ Label "Spend 2 additional resources to discard" [SpendResources iid 2, toDiscardBy iid (attrs.ability 1) attrs] | resources >= 2 ])
      pure e
    _ -> LanternClubMember <$> liftRunMessage msg attrs
