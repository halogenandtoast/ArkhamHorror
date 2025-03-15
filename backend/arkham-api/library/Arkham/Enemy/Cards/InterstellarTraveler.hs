module Arkham.Enemy.Cards.InterstellarTraveler (interstellarTraveler) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait

newtype InterstellarTraveler = InterstellarTraveler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interstellarTraveler :: EnemyCard InterstellarTraveler
interstellarTraveler =
  enemyWith InterstellarTraveler Cards.interstellarTraveler (4, Static 3, 2) (1, 2)
    $ spawnAtL
    ?~ SpawnAt (LocationWithTrait Extradimensional)

instance HasAbilities InterstellarTraveler where
  getAbilities (InterstellarTraveler a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEnters #when Anywhere (be a)

instance RunMessage InterstellarTraveler where
  runMessage msg e@(InterstellarTraveler attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      field EnemyLocation attrs.id >>= traverse_ \loc -> do
        placeDoom (attrs.ability 1) attrs 1
        clueCount <- field LocationClues loc
        when (clueCount > 0) (removeClues (attrs.ability 1) loc 1)
      pure e
    _ -> InterstellarTraveler <$> liftRunMessage msg attrs
