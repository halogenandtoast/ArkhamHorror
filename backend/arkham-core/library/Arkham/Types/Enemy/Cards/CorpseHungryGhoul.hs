module Arkham.Types.Enemy.Cards.CorpseHungryGhoul
  ( corpseHungryGhoul
  , CorpseHungryGhoul(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.LocationMatcher
import Arkham.Types.Message

newtype CorpseHungryGhoul = CorpseHungryGhoul EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corpseHungryGhoul :: EnemyCard CorpseHungryGhoul
corpseHungryGhoul =
  enemy CorpseHungryGhoul Cards.corpseHungryGhoul (4, Static 3, 3) (2, 2)

instance HasModifiersFor env CorpseHungryGhoul

instance ActionRunner env => HasActions env CorpseHungryGhoul where
  getActions i window (CorpseHungryGhoul attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env CorpseHungryGhoul where
  runMessage msg e@(CorpseHungryGhoul attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAt (Just iid) enemyId (LocationWithTitle "Bedroom")
    _ -> CorpseHungryGhoul <$> runMessage msg attrs
