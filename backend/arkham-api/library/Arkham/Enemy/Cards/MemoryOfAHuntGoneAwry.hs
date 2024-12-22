module Arkham.Enemy.Cards.MemoryOfAHuntGoneAwry (memoryOfAHuntGoneAwry) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype MemoryOfAHuntGoneAwry = MemoryOfAHuntGoneAwry EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfAHuntGoneAwry :: EnemyCard MemoryOfAHuntGoneAwry
memoryOfAHuntGoneAwry =
  enemyWith MemoryOfAHuntGoneAwry Cards.memoryOfAHuntGoneAwry (5, PerPlayer 3, 2) (1, 1)
    $ preyL
    .~ Prey (ControlsAsset $ assetIs Assets.roaldEllsworthIntrepidExplorer)

instance HasAbilities MemoryOfAHuntGoneAwry where
  getAbilities (MemoryOfAHuntGoneAwry a) =
    extend
      a
      [ mkAbility a 1
          $ freeReaction
          $ SkillTestResult #after You (WhileEvadingAnEnemy $ be a) (SuccessResult $ atLeast 3)
      , mkAbility a 2 $ forced $ EnemyDefeated #when You ByAny (be a)
      ]

instance RunMessage MemoryOfAHuntGoneAwry where
  runMessage msg e@(MemoryOfAHuntGoneAwry attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      nonAttackEnemyDamage (attrs.ability 1) 3 attrs.id
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Stories.memoryOfAHuntGoneAwry
      pure e
    _ -> MemoryOfAHuntGoneAwry <$> liftRunMessage msg attrs
