module Arkham.Enemy.Cards.MemoryOfAnAlienTranslation (memoryOfAnAlienTranslation) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype MemoryOfAnAlienTranslation = MemoryOfAnAlienTranslation EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfAnAlienTranslation :: EnemyCard MemoryOfAnAlienTranslation
memoryOfAnAlienTranslation =
  enemyWith MemoryOfAnAlienTranslation Cards.memoryOfAnAlienTranslation (3, PerPlayer 4, 5) (1, 1)
    $ preyL
    .~ Prey (ControlsAsset $ assetIs Assets.roaldEllsworthIntrepidExplorer)

instance HasAbilities MemoryOfAnAlienTranslation where
  getAbilities (MemoryOfAnAlienTranslation a) =
    extend
      a
      [ restricted a 1 (thisExists a $ EnemyAt LocationWithAttachment)
          $ freeReaction
          $ EnemyExhausts #after (be a)
      , mkAbility a 2 $ forced $ EnemyDefeated #when You ByAny (be a)
      ]

instance RunMessage MemoryOfAnAlienTranslation where
  runMessage msg e@(MemoryOfAnAlienTranslation attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      nonAttackEnemyDamage (attrs.ability 1) 4 attrs.id
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Stories.memoryOfAnAlienTranslation
      pure e
    _ -> MemoryOfAnAlienTranslation <$> liftRunMessage msg attrs
