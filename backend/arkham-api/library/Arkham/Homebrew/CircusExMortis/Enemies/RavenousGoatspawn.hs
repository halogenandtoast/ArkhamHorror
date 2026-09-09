module Arkham.Homebrew.CircusExMortis.Enemies.RavenousGoatspawn (ravenousGoatspawn) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Matcher

newtype RavenousGoatspawn = RavenousGoatspawn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ravenousGoatspawn :: EnemyCard RavenousGoatspawn
ravenousGoatspawn = enemy RavenousGoatspawn Cards.ravenousGoatspawn

instance HasAbilities RavenousGoatspawn where
  getAbilities (RavenousGoatspawn a) =
    extend
      a
      [ mkAbility a 1
          $ forced
          $ EnemyAttacks #after (You <> hasSealedMoonToken) AnyEnemyAttack (be a)
      , mkAbility a 2
          $ triggered
            (EnemyTakeDamage #after AnyDamageEffect (be a) (atLeast 1) AnySource)
            (GroupClueCostRange (1, 3) Anywhere)
      ]

instance RunMessage RavenousGoatspawn where
  runMessage msg e@(RavenousGoatspawn attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseResources iid (attrs.ability 1) 2
      pure e
    UseCardAbility iid (isSource attrs -> True) 2 _ (totalCluePayment -> x) -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 2) x attrs
      pure e
    _ -> RavenousGoatspawn <$> liftRunMessage msg attrs
