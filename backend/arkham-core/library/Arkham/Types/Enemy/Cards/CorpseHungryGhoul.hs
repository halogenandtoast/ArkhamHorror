module Arkham.Types.Enemy.Cards.CorpseHungryGhoul where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher
import Arkham.Types.Message

newtype CorpseHungryGhoul = CorpseHungryGhoul EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corpseHungryGhoul :: EnemyCard CorpseHungryGhoul
corpseHungryGhoul = enemy CorpseHungryGhoul Cards.corpseHungryGhoul
  $ (healthDamageL .~ 2)
  . (sanityDamageL .~ 2)
  . (fightL .~ 4)
  . (healthL .~ Static 3)
  . (evadeL .~ 3)

instance HasModifiersFor env CorpseHungryGhoul where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env CorpseHungryGhoul where
  getActions i window (CorpseHungryGhoul attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env CorpseHungryGhoul where
  runMessage msg e@(CorpseHungryGhoul attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAt (Just iid) enemyId (LocationWithTitle "Bedroom")
    _ -> CorpseHungryGhoul <$> runMessage msg attrs
