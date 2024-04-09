module Arkham.Enemy.Cards.VengefulHound (vengefulHound, VengefulHound (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype VengefulHound = VengefulHound EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

vengefulHound :: EnemyCard VengefulHound
vengefulHound =
  enemyWith
    VengefulHound
    Cards.vengefulHound
    (2, Static 2, 3)
    (1, 1)
    (\a -> a & preyL .~ BearerOf (toId a))

instance HasModifiersFor VengefulHound where
  getModifiersFor (InvestigatorTarget iid) (VengefulHound attrs) = do
    affected <- iid <=~> investigatorEngagedWith (toId attrs)
    pure
      $ toModifiers attrs
      $ guard affected
      *> [CannotRevealCards, CannotDrawCardsFromPlayerCardEffects]
  getModifiersFor _ _ = pure []

instance RunMessage VengefulHound where
  runMessage msg (VengefulHound attrs) =
    VengefulHound <$> runMessage msg attrs
