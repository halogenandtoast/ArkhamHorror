module Arkham.Enemy.Cards.RiverHawthorne (riverHawthorne) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Message.Lifted.Placement

newtype RiverHawthorne = RiverHawthorne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverHawthorne :: EnemyCard RiverHawthorne
riverHawthorne = enemy RiverHawthorne Cards.riverHawthorne (1, Static 3, 4) (0, 1)

-- TODO: "The first treachery drawn at River Hawthorne's location each round
-- gains surge." Needs a once-per-round modifier granting Surge to the first
-- treachery drawn at her location.
instance HasAbilities RiverHawthorne where
  getAbilities (RiverHawthorne a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage RiverHawthorne where
  runMessage msg e@(RiverHawthorne attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #willpower (Fixed 5)
      pure e
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      place attrs (OutOfPlay SetAsideZone)
      pure e
    _ -> RiverHawthorne <$> liftRunMessage msg attrs
