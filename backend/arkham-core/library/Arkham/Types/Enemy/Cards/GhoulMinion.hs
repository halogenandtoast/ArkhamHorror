{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.GhoulMinion where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype GhoulMinion = GhoulMinion Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ghoulMinion :: EnemyId -> GhoulMinion
ghoulMinion uuid =
  GhoulMinion
    $ baseAttrs uuid "01160"
    $ (healthDamage .~ 1)
    . (sanityDamage .~ 1)
    . (fight .~ 2)
    . (health .~ Static 2)
    . (evade .~ 2)

instance HasModifiersFor env GhoulMinion where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env GhoulMinion where
  getActions i window (GhoulMinion attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GhoulMinion where
  runMessage msg (GhoulMinion attrs) = GhoulMinion <$> runMessage msg attrs
