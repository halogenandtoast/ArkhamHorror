module Arkham.Enemy.Cards.ApexStrangleweed (apexStrangleweed) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype ApexStrangleweed = ApexStrangleweed EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

apexStrangleweed :: EnemyCard ApexStrangleweed
apexStrangleweed = enemy ApexStrangleweed Cards.apexStrangleweed (3, Static 6, 3) (1, 1)

instance HasAbilities ApexStrangleweed where
  getAbilities (ApexStrangleweed a) =
    extend a [mkAbility a 1 $ forced $ EnemyAttacks #after You AttackOfOpportunityAttack $ be a]

instance RunMessage ApexStrangleweed where
  runMessage msg e@(ApexStrangleweed attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasPocketknife <- getHasSupply iid Pocketknife
      unless hasPocketknife do
        setActions iid (attrs.ability 1) 0
        endYourTurn iid
      pure e
    _ -> ApexStrangleweed <$> liftRunMessage msg attrs
