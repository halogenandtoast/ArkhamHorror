module Arkham.Enemy.Cards.ApexStrangleweed (apexStrangleweed, ApexStrangleweed (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype ApexStrangleweed = ApexStrangleweed EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

apexStrangleweed :: EnemyCard ApexStrangleweed
apexStrangleweed = enemy ApexStrangleweed Cards.apexStrangleweed (3, Static 6, 3) (1, 1)

instance HasAbilities ApexStrangleweed where
  getAbilities (ApexStrangleweed a) =
    extend a [mkAbility a 1 $ forced $ EnemyAttacks #after You AttackOfOpportunityAttack $ be a]

instance RunMessage ApexStrangleweed where
  runMessage msg e@(ApexStrangleweed attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      hasPocketknife <- getHasSupply iid Pocketknife
      unless hasPocketknife
        $ pushAll [SetActions iid (toSource attrs) 0, ChooseEndTurn iid]
      pure e
    _ -> ApexStrangleweed <$> runMessage msg attrs
