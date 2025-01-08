module Arkham.Enemy.Cards.MemoryOfARegretfulVoyage (memoryOfARegretfulVoyage) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks, EnemyDefeated)
import Arkham.Helpers.Story
import Arkham.Helpers.Window (cardsDrawn)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype MemoryOfARegretfulVoyage = MemoryOfARegretfulVoyage EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfARegretfulVoyage :: EnemyCard MemoryOfARegretfulVoyage
memoryOfARegretfulVoyage =
  enemyWith MemoryOfARegretfulVoyage Cards.memoryOfARegretfulVoyage (2, PerPlayer 5, 5) (0, 1)
    $ preyL
    .~ Prey (ControlsAsset $ assetIs Assets.professorWilliamDyerProfessorOfGeology)

instance HasAbilities MemoryOfARegretfulVoyage where
  getAbilities (MemoryOfARegretfulVoyage a) =
    extend
      a
      [ restricted a 1 (thisExists a ExhaustedEnemy) $ freeReaction $ DrawsCards #after You (atLeast 1)
      , mkAbility a 2 $ forced $ EnemyDefeated #when You ByAny (be a)
      ]

instance RunMessage MemoryOfARegretfulVoyage where
  runMessage msg e@(MemoryOfARegretfulVoyage attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (cardsDrawn -> length -> n) _ -> do
      nonAttackEnemyDamage (attrs.ability 1) n attrs.id
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Stories.memoryOfARegretfulVoyage
      pure e
    _ -> MemoryOfARegretfulVoyage <$> liftRunMessage msg attrs
