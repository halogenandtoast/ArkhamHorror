module Arkham.Enemy.Cards.DarkYoungHost (darkYoungHost) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Trait
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype DarkYoungHost = DarkYoungHost EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkYoungHost :: EnemyCard DarkYoungHost
darkYoungHost =
  enemyWith
    DarkYoungHost
    Cards.darkYoungHost
    (4, Static 5, 2)
    (2, 2)
    $ spawnAtL
    ?~ SpawnAt (LocationWithTrait Bayou)

instance HasAbilities DarkYoungHost where
  getAbilities (DarkYoungHost a) =
    extend
      a
      [ mkAbility a 1
          $ forced
          $ PlacedCounterOnLocation #when (locationWithEnemy a) AnySource #clue (atLeast 1)
      , mkAbility a 2 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage DarkYoungHost where
  runMessage msg e@(DarkYoungHost attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 [(windowType -> Window.PlacedClues _ target n)] _ -> do
      removeClues (attrs.ability 1) target n
      placeClues (attrs.ability 1) attrs n
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      withLocationOf attrs \loc -> placeClues (attrs.ability 2) loc (enemyClues attrs)
      pure e
    _ -> DarkYoungHost <$> liftRunMessage msg attrs
