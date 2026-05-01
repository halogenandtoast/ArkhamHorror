module Arkham.Enemy.Cards.WhippoorwillUnionAndDisillusion (whippoorwillUnionAndDisillusion) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Cards.Whippoorwill
import Arkham.Enemy.Import.Lifted

newtype WhippoorwillUnionAndDisillusion = WhippoorwillUnionAndDisillusion Whippoorwill
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities, HasModifiersFor)

whippoorwillUnionAndDisillusion :: EnemyCard WhippoorwillUnionAndDisillusion
whippoorwillUnionAndDisillusion =
  enemy
    (WhippoorwillUnionAndDisillusion . Whippoorwill)
    Cards.whippoorwillUnionAndDisillusion
    (2, Static 1, 4)
    (0, 1)
