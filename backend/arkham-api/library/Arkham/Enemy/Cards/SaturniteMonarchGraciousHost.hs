module Arkham.Enemy.Cards.SaturniteMonarchGraciousHost (saturniteMonarchGraciousHost) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Token

newtype SaturniteMonarchGraciousHost = SaturniteMonarchGraciousHost EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saturniteMonarchGraciousHost :: EnemyCard SaturniteMonarchGraciousHost
saturniteMonarchGraciousHost = enemy SaturniteMonarchGraciousHost Cards.saturniteMonarchGraciousHost (2, PerPlayer 4, 4) (2, 2)

instance HasAbilities SaturniteMonarchGraciousHost where
  getAbilities (SaturniteMonarchGraciousHost a) = extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage SaturniteMonarchGraciousHost where
  runMessage msg e@(SaturniteMonarchGraciousHost attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 4)
      pure e
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      placeTokens (attrs.ability 1) attrs #resource 1
      doStep 1 msg
      pure e
    DoStep 1 (PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True)) -> do
      n <- perPlayer 1
      when (countTokens #resource attrs.tokens >= n) $ addToVictory attrs
      pure e
    Flip _ _ (isTarget attrs -> True) -> do
      let inAnAlienLand = lookupCard Cards.saturniteMonarchInAnAlienLand attrs.cardId
      push $ ReplaceEnemy attrs.id inAnAlienLand Swap
      pure e
    _ -> SaturniteMonarchGraciousHost <$> liftRunMessage msg attrs
