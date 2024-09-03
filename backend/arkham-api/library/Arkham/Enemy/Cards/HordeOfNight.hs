module Arkham.Enemy.Cards.HordeOfNight (hordeOfNight, HordeOfNight (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype HordeOfNight = HordeOfNight EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hordeOfNight :: EnemyCard HordeOfNight
hordeOfNight = enemy HordeOfNight Cards.hordeOfNight (1, Static 1, 1) (1, 1)

instance HasModifiersFor HordeOfNight where
  getModifiersFor target (HordeOfNight a) | a `is` target = do
    isHost <- toId a <=~> IsHost
    pure $ toModifiers a $ CannotAttack : [ExhaustIfDefeated | isHost]
  getModifiersFor _ _ = pure []

instance RunMessage HordeOfNight where
  runMessage msg (HordeOfNight attrs) =
    HordeOfNight <$> runMessage msg attrs
