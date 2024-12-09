module Arkham.Location.Cards.MysteriousStairs_187 (mysteriousStairs_187, MysteriousStairs_187 (..)) where

import Arkham.Direction
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype MysteriousStairs_187 = MysteriousStairs_187 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousStairs_187 :: LocationCard MysteriousStairs_187
mysteriousStairs_187 =
  locationWith MysteriousStairs_187 Cards.mysteriousStairs_187 3 (Static 0)
    $ connectsToL
    .~ setFromList [Above, Below]

instance HasModifiersFor MysteriousStairs_187 where
  getModifiersFor (MysteriousStairs_187 a) = modifySelectMaybe a (investigatorAt a) \_ -> do
    theUnnamable <- MaybeT $ selectOne $ enemyIs Enemies.theUnnamable <> enemyAt a
    n <- lift $ perPlayer 1
    liftGuardM $ fieldMap EnemyDamage (< n) theUnnamable
    pure [CannotTakeAction #move, CannotTakeAction #resign]

instance HasAbilities MysteriousStairs_187 where
  getAbilities (MysteriousStairs_187 attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 (exists $ enemyIs Enemies.theUnnamable <> not_ (enemyAt attrs))
      $ forced
      $ RevealLocation #when Anyone (be attrs)

instance RunMessage MysteriousStairs_187 where
  runMessage msg l@(MysteriousStairs_187 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectForMaybeM (enemyIs Enemies.theUnnamable) \theUnnamable -> do
        push $ EnemyMove theUnnamable attrs.id
      pure l
    _ -> MysteriousStairs_187 <$> runMessage msg attrs
