module Arkham.Enemy.Cards.ChildrenOfBlood.NewHorizons.NightWatchman (nightWatchman) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype NightWatchman = NightWatchman EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nightWatchman :: EnemyCard NightWatchman
nightWatchman = enemy NightWatchman Cards.nightWatchman & setPrey MostClues

instance RunMessage NightWatchman where
  runMessage msg (NightWatchman attrs) = NightWatchman <$> runMessage msg attrs
