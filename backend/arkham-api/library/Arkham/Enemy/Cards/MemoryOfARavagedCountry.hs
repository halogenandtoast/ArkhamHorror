module Arkham.Enemy.Cards.MemoryOfARavagedCountry (memoryOfARavagedCountry) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks, EnemyDefeated)
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype MemoryOfARavagedCountry = MemoryOfARavagedCountry EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfARavagedCountry :: EnemyCard MemoryOfARavagedCountry
memoryOfARavagedCountry =
  enemyWith MemoryOfARavagedCountry Cards.memoryOfARavagedCountry (5, PerPlayer 3, 3) (2, 1)
    $ preyL
    .~ Prey (ControlsAsset $ assetIs Assets.jamesCookieFredericksDubiousChoice)

instance HasAbilities MemoryOfARavagedCountry where
  getAbilities (MemoryOfARavagedCountry a) =
    extend
      a
      [ mkAbility a 1 $ freeReaction $ EnemyAttacks #after You AnyEnemyAttack (be a)
      , mkAbility a 2 $ forced $ EnemyDefeated #when You ByAny (be a)
      ]

instance RunMessage MemoryOfARavagedCountry where
  runMessage msg e@(MemoryOfARavagedCountry attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      nonAttackEnemyDamage (attrs.ability 1) 1 attrs.id
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Stories.memoryOfARavagedCountry
      pure e
    _ -> MemoryOfARavagedCountry <$> liftRunMessage msg attrs
