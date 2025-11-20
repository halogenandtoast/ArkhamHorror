module Arkham.Enemy.Cards.ZadokAllenDrunkAndDisorderly (zadokAllenDrunkAndDisorderly) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Helpers.SkillTest.Lifted

newtype ZadokAllenDrunkAndDisorderly = ZadokAllenDrunkAndDisorderly EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zadokAllenDrunkAndDisorderly :: EnemyCard ZadokAllenDrunkAndDisorderly
zadokAllenDrunkAndDisorderly = enemy ZadokAllenDrunkAndDisorderly Cards.zadokAllenDrunkAndDisorderly (4, Static 5, 2) (2, 0)

instance HasAbilities ZadokAllenDrunkAndDisorderly where
  getAbilities (ZadokAllenDrunkAndDisorderly a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage ZadokAllenDrunkAndDisorderly where
  runMessage msg e@(ZadokAllenDrunkAndDisorderly attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeClues attrs attrs =<< perPlayer 1
      pure $ updateAttrs e (spawnAtL ?~ "Fish Street Bridge")
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getId
      parley sid iid (attrs.ability 1) attrs #agility (Fixed 3)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      moveTokens (attrs.ability 1) attrs iid #clue 1
      doStep 2 msg
      pure e
    DoStep 2 (PassedThisSkillTest iid (isAbilitySource attrs 1 -> True)) -> do
      when (attrs.token #clue == 0) $ addToVictory iid attrs
      pure e
    _ -> ZadokAllenDrunkAndDisorderly <$> liftRunMessage msg attrs
