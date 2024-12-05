module Arkham.Enemy.Cards.HitVan (hitVan, HitVan (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Matcher

newtype HitVan = HitVan EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hitVan :: EnemyCard HitVan
hitVan = enemyWith HitVan Cards.hitVan (3, Static 5, 3) (1, 1) (spawnAtL ?~ SpawnAt RearmostLocation)

instance HasModifiersFor HitVan where
  getModifiersFor (HitVan a) =
    modifySelfWhen a (enemyMovedFromHunterKeyword a) [CannotAttack]

instance RunMessage HitVan where
  runMessage msg (HitVan attrs) = runQueueT $ case msg of
    _ -> HitVan <$> liftRunMessage msg attrs
