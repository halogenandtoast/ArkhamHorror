module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.DiningHall (diningHall) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DiningHall = DiningHall LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diningHall :: LocationCard DiningHall
diningHall = symbolLabel $ location DiningHall Cards.diningHall 3 (PerPlayer 1)

instance HasModifiersFor DiningHall where
  getModifiersFor (DiningHall a) =
    modifySelectWhen a a.revealed (enemyAt a) [EnemyFight 1, EnemyEvade (-1)]

instance HasAbilities DiningHall where
  getAbilities (DiningHall a) =
    extendRevealed1 a
      $ playerLimit PerTurn
      $ restricted a 1 Here
      $ freeReaction
      $ SkillTestResult #after You (WhileEvadingAnEnemy $ enemyAt a) (SuccessResult $ atLeast 2)

instance RunMessage DiningHall where
  runMessage msg l@(DiningHall attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      whenJustM getSkillTestTargetedEnemy \eid -> nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 eid
      pure l
    _ -> DiningHall <$> liftRunMessage msg attrs
