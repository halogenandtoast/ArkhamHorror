module Arkham.Enemy.Cards.VengefulSerpent (vengefulSerpent) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype VengefulSerpent = VengefulSerpent EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengefulSerpent :: EnemyCard VengefulSerpent
vengefulSerpent = enemy VengefulSerpent Cards.vengefulSerpent (2, Static 2, 2) (1, 1)

instance HasAbilities VengefulSerpent where
  getAbilities (VengefulSerpent a) =
    extend1 a
      $ restricted a 1 (exists $ VictoryDisplayCardMatch $ basic $ cardIs Cards.vengefulSerpent)
      $ forced
      $ EnemySpawns #after Anywhere (be a)

instance RunMessage VengefulSerpent where
  runMessage msg e@(VengefulSerpent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      serpents <- select $ VictoryDisplayCardMatch $ basic $ cardIs Cards.vengefulSerpent
      for_ serpents obtainCard
      for_ serpents (drawCard iid)
      pure e
    _ -> VengefulSerpent <$> liftRunMessage msg attrs
