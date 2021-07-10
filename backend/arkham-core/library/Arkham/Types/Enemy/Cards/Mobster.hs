module Arkham.Types.Enemy.Cards.Mobster
  ( mobster
  , Mobster(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message

newtype Mobster = Mobster EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mobster :: EnemyCard Mobster
mobster = enemy Mobster Cards.mobster (2, Static 2, 2) (1, 0)

instance HasModifiersFor env Mobster

instance ActionRunner env => HasActions env Mobster where
  getActions i window (Mobster attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env Mobster where
  runMessage msg e@(Mobster attrs@EnemyAttrs {..}) = case msg of
    After (PerformEnemyAttack iid eid) | eid == enemyId ->
      e <$ push (SpendResources iid 1)
    _ -> Mobster <$> runMessage msg attrs
