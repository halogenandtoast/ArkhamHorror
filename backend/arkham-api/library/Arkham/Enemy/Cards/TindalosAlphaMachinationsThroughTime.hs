module Arkham.Enemy.Cards.TindalosAlphaMachinationsThroughTime (
  tindalosAlphaMachinationsThroughTime,
) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TindalosAlphaMachinationsThroughTime = TindalosAlphaMachinationsThroughTime EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tindalosAlphaMachinationsThroughTime :: EnemyCard TindalosAlphaMachinationsThroughTime
tindalosAlphaMachinationsThroughTime =
  enemy
    TindalosAlphaMachinationsThroughTime
    Cards.tindalosAlphaMachinationsThroughTime

instance HasAbilities TindalosAlphaMachinationsThroughTime where
  getAbilities (TindalosAlphaMachinationsThroughTime a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)
      , mkAbility a 2 $ forced $ PhaseEnds #when #enemy
      ]

instance RunMessage TindalosAlphaMachinationsThroughTime where
  runMessage msg e@(TindalosAlphaMachinationsThroughTime attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid <> AssetNonStory <> AssetCanLeavePlayByNormalMeans
      chooseTargetM iid assets \asset -> do
        moveAllTokens (attrs.ability 1) asset iid #damage
        moveAllTokens (attrs.ability 1) asset iid #horror
        shuffleIntoDeck iid asset

      toDiscard (attrs.ability 1) attrs
      pure e
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure e
    _ -> TindalosAlphaMachinationsThroughTime <$> liftRunMessage msg attrs
