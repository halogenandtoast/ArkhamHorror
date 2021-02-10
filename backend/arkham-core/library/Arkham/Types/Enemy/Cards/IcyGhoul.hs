module Arkham.Types.Enemy.Cards.IcyGhoul where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype IcyGhoul = IcyGhoul EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

icyGhoul :: EnemyId -> IcyGhoul
icyGhoul uuid =
  IcyGhoul
    $ baseAttrs uuid "01119"
    $ (healthDamageL .~ 2)
    . (sanityDamageL .~ 1)
    . (fightL .~ 3)
    . (healthL .~ Static 4)
    . (evadeL .~ 4)

instance HasModifiersFor env IcyGhoul where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env IcyGhoul where
  getActions i window (IcyGhoul attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env IcyGhoul where
  runMessage msg e@(IcyGhoul attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAt (Just iid) enemyId (LocationWithTitle "Cellar")
    _ -> IcyGhoul <$> runMessage msg attrs
