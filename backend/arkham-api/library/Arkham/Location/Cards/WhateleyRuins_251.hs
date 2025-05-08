module Arkham.Location.Cards.WhateleyRuins_251 (whateleyRuins_251) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Enemy
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards (whateleyRuins_251)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers

newtype WhateleyRuins_251 = WhateleyRuins_251 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whateleyRuins_251 :: LocationCard WhateleyRuins_251
whateleyRuins_251 = location WhateleyRuins_251 Cards.whateleyRuins_251 2 (PerPlayer 2)

instance HasModifiersFor WhateleyRuins_251 where
  getModifiersFor (WhateleyRuins_251 attrs) = do
    modifySelect attrs (investigatorAt attrs) [SkillModifier #willpower (-1)]

instance HasAbilities WhateleyRuins_251 where
  getAbilities (WhateleyRuins_251 a) = extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage WhateleyRuins_251 where
  runMessage msg l@(WhateleyRuins_251 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      abominations <- getBroodOfYogSothoth
      chooseTargetM iid abominations \abomination -> do
        destinations <- getEnemyAccessibleLocations abomination
        chooseTargetM iid destinations $ enemyMoveTo abomination
      pure l
    _ -> WhateleyRuins_251 <$> liftRunMessage msg attrs
