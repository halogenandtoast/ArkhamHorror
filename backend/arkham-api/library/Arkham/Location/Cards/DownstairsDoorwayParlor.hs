module Arkham.Location.Cards.DownstairsDoorwayParlor (
  downstairsDoorwayParlor,
  DownstairsDoorwayParlor (..),
) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype DownstairsDoorwayParlor = DownstairsDoorwayParlor LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downstairsDoorwayParlor :: LocationCard DownstairsDoorwayParlor
downstairsDoorwayParlor = location DownstairsDoorwayParlor Cards.downstairsDoorwayParlor 2 (PerPlayer 1)

instance HasModifiersFor DownstairsDoorwayParlor where
  getModifiersFor (DownstairsDoorwayParlor a) = do
    n <- selectCount $ investigatorAt a
    modifySelf a [ShroudModifier n]

instance HasAbilities DownstairsDoorwayParlor where
  getAbilities (DownstairsDoorwayParlor a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( Here
            <> exists (enemyIs Enemies.theUnnamable <> EnemyWithDamage (AtLeast $ PerPlayer 1))
            <> not_ (Remembered RecoveredAStrangeKey)
        )
      $ FastAbility Free

instance RunMessage DownstairsDoorwayParlor where
  runMessage msg l@(DownstairsDoorwayParlor attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember RecoveredAStrangeKey
      pure l
    _ -> DownstairsDoorwayParlor <$> liftRunMessage msg attrs
