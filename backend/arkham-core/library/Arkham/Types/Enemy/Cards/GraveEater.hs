module Arkham.Types.Enemy.Cards.GraveEater where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.GameValue
import Arkham.Types.Message

newtype GraveEater = GraveEater EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveEater :: EnemyCard GraveEater
graveEater = enemy GraveEater Cards.graveEater
  $ (healthDamageL .~ 1)
  . (sanityDamageL .~ 1)
  . (fightL .~ 2)
  . (healthL .~ Static 2)
  . (evadeL .~ 2)

instance HasModifiersFor env GraveEater where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env GraveEater where
  getActions i window (GraveEater attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GraveEater where
  runMessage msg e@(GraveEater attrs) = case msg of
    After (EnemyAttack iid eid) | eid == enemyId attrs ->
      e <$ unshiftMessage (RandomDiscard iid)
    _ -> GraveEater <$> runMessage msg attrs
