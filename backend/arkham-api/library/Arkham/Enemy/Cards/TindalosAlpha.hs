module Arkham.Enemy.Cards.TindalosAlpha (tindalosAlpha) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TindalosAlpha = TindalosAlpha EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tindalosAlpha :: EnemyCard TindalosAlpha
tindalosAlpha = enemy TindalosAlpha Cards.tindalosAlpha (4, Static 3, 4) (1, 1)

instance HasAbilities TindalosAlpha where
  getAbilities (TindalosAlpha a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)
      , mkAbility a 2 $ forced $ PhaseEnds #when #enemy
      ]

instance RunMessage TindalosAlpha where
  runMessage msg e@(TindalosAlpha attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid <> AssetNonStory <> AssetCanLeavePlayByNormalMeans
      chooseTargetM iid assets \asset -> do
        moveAllTokens (attrs.ability 1) asset iid #damage
        moveAllTokens (attrs.ability 1) asset iid #horror
        toDiscard (attrs.ability 1) asset

      toDiscard (attrs.ability 1) attrs
      pure e
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure e
    _ -> TindalosAlpha <$> liftRunMessage msg attrs
