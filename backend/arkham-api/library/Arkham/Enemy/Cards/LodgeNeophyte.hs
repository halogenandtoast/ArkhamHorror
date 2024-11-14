module Arkham.Enemy.Cards.LodgeNeophyte (lodgeNeophyte, LodgeNeophyte (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher

newtype LodgeNeophyte = LodgeNeophyte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeNeophyte :: EnemyCard LodgeNeophyte
lodgeNeophyte =
  enemyWith LodgeNeophyte Cards.lodgeNeophyte (3, Static 1, 2) (0, 1)
    $ spawnAtL
    ?~ SpawnAt EmptyLocation

instance HasAbilities LodgeNeophyte where
  getAbilities (LodgeNeophyte a) =
    withBaseAbilities
      a
      [ restricted a 1 CanPlaceDoomOnThis $ forced $ EnemySpawns #after Anywhere (be a)
      , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
      ]

instance RunMessage LodgeNeophyte where
  runMessage msg e@(LodgeNeophyte attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 2) attrs #willpower (Fixed 2)
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      push $ RemoveAllDoom (attrs.ability 2) (toTarget attrs)
      pure e
    _ -> LodgeNeophyte <$> liftRunMessage msg attrs
