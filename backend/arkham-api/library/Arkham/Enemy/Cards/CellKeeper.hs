module Arkham.Enemy.Cards.CellKeeper (cellKeeper) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.I18n
import Arkham.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ForTheGreaterGood.Helpers
import Arkham.Trait (Trait (Sanctum))

newtype CellKeeper = CellKeeper EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cellKeeper :: EnemyCard CellKeeper
cellKeeper =
  enemy CellKeeper Cards.cellKeeper (3, Static 3, 2) (0, 2)
    & setSpawnAt (LocationWithTrait Sanctum)

instance HasAbilities CellKeeper where
  getAbilities (CellKeeper a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemySpawns #after Anywhere (be a)
      , restricted a 2 keyCriteria
          $ forced
          $ SkillTestResult #after You (WhileEvadingAnEnemy $ be a) (SuccessResult $ atLeast 2)
      ]
   where
    keyCriteria = if null (enemyKeys a) then Never else NoRestriction

instance RunMessage CellKeeper where
  runMessage msg e@(CellKeeper attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 2
      mKey <- getRandomKey
      for_ mKey (placeKey attrs)
      pure e
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      chooseOrRunOneM iid $ withI18n do
        for_ (setToList $ enemyKeys attrs) \k ->
          withVar "name" (String $ keyName k) $ labeled' "takeControlOfNamedKey" $ placeKey iid k
      pure e
    _ -> CellKeeper <$> liftRunMessage msg attrs
