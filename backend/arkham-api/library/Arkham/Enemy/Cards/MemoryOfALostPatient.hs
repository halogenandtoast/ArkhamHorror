module Arkham.Enemy.Cards.MemoryOfALostPatient (memoryOfALostPatient) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Story
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype MemoryOfALostPatient = MemoryOfALostPatient EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfALostPatient :: EnemyCard MemoryOfALostPatient
memoryOfALostPatient =
  enemyWith MemoryOfALostPatient Cards.memoryOfALostPatient (3, PerPlayer 4, 4) (1, 1)
    $ preyL
    .~ Prey (ControlsAsset $ assetIs Assets.drMalaSinhaDaringPhysician)

instance HasAbilities MemoryOfALostPatient where
  getAbilities (MemoryOfALostPatient a) =
    extend
      a
      [ restricted a 1 (thisExists a ExhaustedEnemy)
          $ freeReaction
          $ oneOf
            [ AssetHealed #after #damage AnyAsset (SourceOwnedBy You)
            , InvestigatorHealed #after #damage Anyone (SourceOwnedBy You)
            ]
      , mkAbility a 2 $ forced $ EnemyDefeated #when You ByAny (be a)
      ]

instance RunMessage MemoryOfALostPatient where
  runMessage msg e@(MemoryOfALostPatient attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (healedAmount -> n) _ -> do
      nonAttackEnemyDamage (attrs.ability 1) n attrs.id
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Stories.memoryOfALostPatient
      pure e
    _ -> MemoryOfALostPatient <$> liftRunMessage msg attrs
