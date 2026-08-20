module Arkham.Enemy.Cards.TheCircleUndone.UnionAndDisillusion.Whippoorwill (whippoorwill) where

import Arkham.Enemy.CardDefs.TheCircleUndone.UnionAndDisillusion qualified as Cards
import Arkham.Enemy.Cards.TheDunwichLegacy.Whippoorwills.Whippoorwill qualified as Base
import Arkham.Enemy.Import.Lifted

newtype Whippoorwill = Whippoorwill Base.Whippoorwill
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities, HasModifiersFor)

whippoorwill :: EnemyCard Whippoorwill
whippoorwill =
  enemy
    (Whippoorwill . Base.Whippoorwill)
    Cards.whippoorwill
