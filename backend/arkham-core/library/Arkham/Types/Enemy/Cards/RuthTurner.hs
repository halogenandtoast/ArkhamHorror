{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.RuthTurner where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype RuthTurner = RuthTurner Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ruthTurner :: EnemyId -> RuthTurner
ruthTurner uuid =
  RuthTurner
    $ baseAttrs uuid "01141"
    $ (healthDamage .~ 1)
    . (fight .~ 2)
    . (health .~ Static 4)
    . (evade .~ 5)

instance HasModifiersFor env RuthTurner where
  getModifiersFor = noModifiersFor

instance HasModifiers env RuthTurner where
  getModifiers _ (RuthTurner Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env RuthTurner where
  getActions i window (RuthTurner attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env RuthTurner where
  runMessage msg e@(RuthTurner attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAt (Just iid) eid "St. Mary's Hospital"
    EnemyEvaded _ eid | eid == enemyId ->
      e <$ unshiftMessage (AddToVictory (EnemyTarget enemyId))
    _ -> RuthTurner <$> runMessage msg attrs
