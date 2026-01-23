module Arkham.Enemy.Cards.CrystalParasite (crystalParasite) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Enemy.Types.Attrs (enemyDamage)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype CrystalParasite = CrystalParasite EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalParasite :: EnemyCard CrystalParasite
crystalParasite = enemy CrystalParasite Cards.crystalParasite (2, Static 6, 2) (1, 1)

instance HasAbilities CrystalParasite where
  getAbilities (CrystalParasite a) =
    extend1 a
      $ restricted a 1 IsDay
      $ forced
      $ EnemyAttacks #after Anyone AnyEnemyAttack (be a <> EnemyWithAnyDamage)

instance HasModifiersFor CrystalParasite where
  getModifiersFor (CrystalParasite a) = do
    time <- getCampaignTime
    let n = enemyDamage a `div` 2
    when (time == Night && n > 0) do
      modifySelf a [DamageDealt n, EnemyFight (n * 2)]

instance RunMessage CrystalParasite where
  runMessage msg e@(CrystalParasite attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 2
      pure e
    _ -> CrystalParasite <$> liftRunMessage msg attrs
