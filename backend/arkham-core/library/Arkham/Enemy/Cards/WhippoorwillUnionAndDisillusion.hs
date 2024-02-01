module Arkham.Enemy.Cards.WhippoorwillUnionAndDisillusion (
  WhippoorwillUnionAndDisillusion (..),
  whippoorwillUnionAndDisillusion,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Cards.Whippoorwill
import Arkham.Enemy.Runner

newtype WhippoorwillUnionAndDisillusion = WhippoorwillUnionAndDisillusion Whippoorwill
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities, HasModifiersFor)

whippoorwillUnionAndDisillusion :: EnemyCard WhippoorwillUnionAndDisillusion
whippoorwillUnionAndDisillusion =
  enemy
    (WhippoorwillUnionAndDisillusion . Whippoorwill)
    Cards.whippoorwillUnionAndDisillusion
    (2, Static 1, 4)
    (0, 1)

instance RunMessage WhippoorwillUnionAndDisillusion where
  runMessage msg (WhippoorwillUnionAndDisillusion inner) = WhippoorwillUnionAndDisillusion <$> runMessage msg inner
