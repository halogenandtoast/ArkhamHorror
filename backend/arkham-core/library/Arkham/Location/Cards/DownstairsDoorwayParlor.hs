module Arkham.Location.Cards.DownstairsDoorwayParlor (downstairsDoorwayParlor, DownstairsDoorwayParlor (..)) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey

newtype DownstairsDoorwayParlor = DownstairsDoorwayParlor LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downstairsDoorwayParlor :: LocationCard DownstairsDoorwayParlor
downstairsDoorwayParlor = location DownstairsDoorwayParlor Cards.downstairsDoorwayParlor 2 (PerPlayer 1)

instance HasModifiersFor DownstairsDoorwayParlor where
  getModifiersFor target (DownstairsDoorwayParlor a) | a `is` target = do
    n <- selectCount $ investigatorAt a
    pure $ toModifiers a [ShroudModifier (-n) | n > 0]
  getModifiersFor _ _ = pure []

instance HasAbilities DownstairsDoorwayParlor where
  getAbilities (DownstairsDoorwayParlor attrs) =
    extendRevealed
      attrs
      [ restrictedAbility
          attrs
          1
          ( Here
              <> exists (enemyIs Enemies.theUnnamable <> EnemyWithDamage (AtLeast $ PerPlayer 1))
              <> not_ (Remembered RecoveredAStrangeKey)
          )
          $ FastAbility Free
      ]

instance RunMessage DownstairsDoorwayParlor where
  runMessage msg l@(DownstairsDoorwayParlor attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ Remember RecoveredAStrangeKey
      pure l
    _ -> DownstairsDoorwayParlor <$> runMessage msg attrs
