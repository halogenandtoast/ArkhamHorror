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
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousStairs_187 :: LocationCard MysteriousStairs_187
mysteriousStairs_187 =
  locationWith
    MysteriousStairs_187
    Cards.mysteriousStairs_187
    3
    (Static 0)
    (connectsToL .~ setFromList [Above, Below])

instance HasModifiersFor MysteriousStairs_187 where
  getModifiersFor (InvestigatorTarget iid) (MysteriousStairs_187 attrs) = do
    here <- iid `isAt` attrs
    n <- perPlayer 1
    mTheUnnamable <- selectOne $ enemyIs Enemies.theUnnamable <> enemyAt attrs
    hasEnoughDamage <- maybe (pure False) (fieldMap EnemyDamage (>= n)) mTheUnnamable
    pure
      $ toModifiers attrs
      $ guard (here && hasEnoughDamage)
      *> [CannotTakeAction #move, CannotTakeAction #resign]
  getModifiersFor _ _ = pure []

instance HasAbilities MysteriousStairs_187 where
  getAbilities (MysteriousStairs_187 attrs) =
    extendRevealed
      attrs
      [mkAbility attrs 1 $ forced $ RevealLocation #when Anyone (be attrs)]

instance RunMessage MysteriousStairs_187 where
  runMessage msg l@(MysteriousStairs_187 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mTheUnnamable <- selectOne $ enemyIs Enemies.theUnnamable
      for_ mTheUnnamable $ \theUnnamable -> push $ EnemyMove theUnnamable attrs.id
      pure l
    _ -> MysteriousStairs_187 <$> runMessage msg attrs
