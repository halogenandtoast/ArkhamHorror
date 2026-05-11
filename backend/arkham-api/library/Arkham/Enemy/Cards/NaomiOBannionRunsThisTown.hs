module Arkham.Enemy.Cards.NaomiOBannionRunsThisTown (naomiOBannionRunsThisTown) where

import Arkham.Ability
import Arkham.Calculation
import Arkham.Campaigns.BrethrenOfAsh.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype NaomiOBannionRunsThisTown = NaomiOBannionRunsThisTown EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

naomiOBannionRunsThisTown :: EnemyCard NaomiOBannionRunsThisTown
naomiOBannionRunsThisTown =
  enemy NaomiOBannionRunsThisTown Cards.naomiOBannionRunsThisTown (4, Static 5, 2) (1, 1)

instance HasAbilities NaomiOBannionRunsThisTown where
  getAbilities (NaomiOBannionRunsThisTown a) =
    extend1 a
      $ restricted a 1 OnSameLocation
      $ parleyAction (CalculatedResourceCost (GameValueCalculation $ PerPlayer 1))

instance RunMessage NaomiOBannionRunsThisTown where
  runMessage msg e@(NaomiOBannionRunsThisTown attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 3
      pure e
    _ -> NaomiOBannionRunsThisTown <$> liftRunMessage msg attrs
