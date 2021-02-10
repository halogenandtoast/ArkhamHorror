module Arkham.Types.Enemy.Cards.GhoulMinion where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype GhoulMinion = GhoulMinion EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghoulMinion :: EnemyId -> GhoulMinion
ghoulMinion uuid =
  GhoulMinion
    $ baseAttrs uuid "01160"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 2)
    . (healthL .~ Static 2)
    . (evadeL .~ 2)

instance HasModifiersFor env GhoulMinion where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env GhoulMinion where
  getActions i window (GhoulMinion attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GhoulMinion where
  runMessage msg (GhoulMinion attrs) = GhoulMinion <$> runMessage msg attrs
