module Arkham.Enemy.Cards.GideonMizrah (gideonMizrah) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype GideonMizrah = GideonMizrah EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gideonMizrah :: EnemyCard GideonMizrah
gideonMizrah = enemy GideonMizrah Cards.gideonMizrah (2, Static 3, 1) (1, 0)

instance HasAbilities GideonMizrah where
  getAbilities (GideonMizrah a) =
    extend
      a
      [ restricted a 1 OnSameLocation (parleyAction (ActionCost 2))
      , mkAbility a 2 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)
      ]

instance RunMessage GideonMizrah where
  runMessage msg e@(GideonMizrah attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      place attrs (OutOfPlay SetAsideZone)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      randomDiscard iid (attrs.ability 2)
      pure e
    _ -> GideonMizrah <$> liftRunMessage msg attrs
