{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.GraveEater where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype GraveEater = GraveEater Attrs
  deriving newtype (Show, ToJSON, FromJSON)

graveEater :: EnemyId -> GraveEater
graveEater uuid =
  GraveEater
    $ baseAttrs uuid "50038"
    $ (healthDamage .~ 1)
    . (sanityDamage .~ 1)
    . (fight .~ 2)
    . (health .~ Static 2)
    . (evade .~ 2)

instance HasModifiersFor env GraveEater where
  getModifiersFor = noModifiersFor

instance HasModifiers env GraveEater where
  getModifiers _ (GraveEater Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env GraveEater where
  getActions i window (GraveEater attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GraveEater where
  runMessage msg e@(GraveEater attrs) = case msg of
    After (EnemyAttack iid eid) | eid == enemyId attrs -> do
      e <$ unshiftMessage (RandomDiscard iid)
    _ -> GraveEater <$> runMessage msg attrs
