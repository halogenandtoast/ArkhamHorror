module Arkham.Enemy.Cards.MemoryOfAnUnspeakableEvil (memoryOfAnUnspeakableEvil) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks, EnemyDefeated)
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Story.Cards qualified as Stories

newtype MemoryOfAnUnspeakableEvil = MemoryOfAnUnspeakableEvil EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfAnUnspeakableEvil :: EnemyCard MemoryOfAnUnspeakableEvil
memoryOfAnUnspeakableEvil =
  enemyWith MemoryOfAnUnspeakableEvil Cards.memoryOfAnUnspeakableEvil (4, PerPlayer 3, 4) (0, 2)
    $ preyL
    .~ Prey (ControlsAsset $ assetIs Assets.danforthBrilliantStudent)

instance HasAbilities MemoryOfAnUnspeakableEvil where
  getAbilities (MemoryOfAnUnspeakableEvil a) =
    extend
      a
      [ restricted a 1 OnSameLocation
          $ ActionAbility [#parley] (ActionCost 2 <> ShuffleTopOfScenarioDeckIntoYourDeck 3 TekeliliDeck)
      , mkAbility a 2 $ forced $ EnemyDefeated #when You ByAny (be a)
      ]

instance RunMessage MemoryOfAnUnspeakableEvil where
  runMessage msg e@(MemoryOfAnUnspeakableEvil attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      nonAttackEnemyDamage (attrs.ability 1) 3 attrs.id
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Stories.memoryOfAnUnspeakableEvil
      pure e
    _ -> MemoryOfAnUnspeakableEvil <$> liftRunMessage msg attrs
