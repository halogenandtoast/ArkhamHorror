module Arkham.Enemy.Cards.HuntingHorror (huntingHorror, HuntingHorror (..)) where

import Arkham.Ability
import Arkham.Classes.HasQueue (withQueue_)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Runner (filterOutEnemyMessages)
import Arkham.Helpers.ChaosBag.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Token

newtype HuntingHorror = HuntingHorror EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingHorror :: EnemyCard HuntingHorror
huntingHorror = enemy HuntingHorror Cards.huntingHorror (2, Static 3, 2) (1, 1)

instance HasAbilities HuntingHorror where
  getAbilities (HuntingHorror x) =
    extend
      x
      [ restricted x 1 (criteria <> exhaustedCriteria) $ forced $ PhaseBegins #when #enemy
      , restricted x 2 criteria $ forced $ EnemyLeavesPlay #when (be x)
      ]
   where
    exhaustedCriteria = if x.ready then Never else NoRestriction
    criteria = case x.placement of
      OutOfPlay VoidZone -> Never
      _ -> NoRestriction

instance RunMessage HuntingHorror where
  runMessage msg e@(HuntingHorror attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      requestChaosTokens iid (attrs.ability 1) 1
      pure e
    RequestedChaosTokens (isSource attrs -> True) (Just iid) (map (.face) -> faces) -> do
      chooseOneM iid do
        labeled "Continue" do
          when (any (`elem` faces) [#skull, #cultist, #tablet, #elderthing, #autofail]) $ readyThis attrs
      resetChaosTokens (attrs.ability 1)
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ PlaceEnemyOutOfPlay VoidZone attrs.id
      pure e
    EnemySpawnFromOutOfPlay VoidZone _miid _lid eid | eid == attrs.id -> do
      pure
        . HuntingHorror
        $ attrs
        & (tokensL %~ removeAllTokens #doom . removeAllTokens #clue . removeAllTokens #damage)
        & (defeatedL .~ False)
        & (exhaustedL .~ False)
    PlaceEnemyOutOfPlay VoidZone eid | eid == attrs.id -> do
      lift $ withQueue_ $ mapMaybe (filterOutEnemyMessages eid)
      pure
        . HuntingHorror
        $ attrs
        & (tokensL %~ removeAllTokens #doom . removeAllTokens #clue . removeAllTokens #damage)
        & (placementL .~ OutOfPlay VoidZone)
        & (defeatedL .~ False)
        & (exhaustedL .~ False)
    _ -> HuntingHorror <$> liftRunMessage msg attrs
