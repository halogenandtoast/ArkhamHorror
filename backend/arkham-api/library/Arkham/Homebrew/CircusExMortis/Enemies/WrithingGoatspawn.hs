module Arkham.Homebrew.CircusExMortis.Enemies.WrithingGoatspawn (writhingGoatspawn) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype WrithingGoatspawn = WrithingGoatspawn EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

writhingGoatspawn :: EnemyCard WrithingGoatspawn
writhingGoatspawn = enemy WrithingGoatspawn Cards.writhingGoatspawn

instance HasModifiersFor WrithingGoatspawn where
  getModifiersFor (WrithingGoatspawn a) =
    modifySelf a [AddKeyword Keyword.Massive, AddKeyword Keyword.Retaliate]

instance HasAbilities WrithingGoatspawn where
  getAbilities (WrithingGoatspawn a) =
    extend
      a
      [ restricted a 1 (thisExists a ReadyEnemy)
          $ forced
          $ Enters #after (You <> hasSealedMoonToken) (LocationWithEnemy (be a))
      , mkAbility a 2
          $ triggered
            (EnemyTakeDamage #after AnyDamageEffect (be a) (atLeast 1) AnySource)
            (GroupClueCostRange (1, 3) Anywhere)
      ]

instance RunMessage WrithingGoatspawn where
  runMessage msg e@(WrithingGoatspawn attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    UseCardAbility iid (isSource attrs -> True) 2 _ (totalCluePayment -> x) -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 2) x attrs
      pure e
    _ -> WrithingGoatspawn <$> liftRunMessage msg attrs
