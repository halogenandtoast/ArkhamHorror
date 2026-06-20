module Arkham.Enemy.Cards.Nassor (nassor) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import {-# SOURCE #-} Arkham.GameEnv (getPhase)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher
import Arkham.Phase

newtype Nassor = Nassor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nassor :: EnemyCard Nassor
nassor =
  enemyWith Nassor Cards.nassor
    $ spawnAtL
    ?~ SpawnAt "Streets of Cairo"

instance HasAbilities Nassor where
  getAbilities (Nassor a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #when Anyone AnyEnemyAttack (be a)

instance RunMessage Nassor where
  runMessage msg e@(Nassor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      phase <- getPhase
      let extra = if phase == EnemyPhase then 2 else 1
      campaignI18n $ chooseOneM iid do
        labeled' "nassor.addStrength" $ addStrengthOfTheAbyss 1
        countVar extra $ labeled' "nassor.extraDamage" do
          enemyAttackModifiers (attrs.ability 1) attrs [DamageDealt extra, HorrorDealt extra]
      pure e
    _ -> Nassor <$> liftRunMessage msg attrs
