{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.GoatSpawn where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype GoatSpawn = GoatSpawn Attrs
  deriving newtype (Show, ToJSON, FromJSON)

goatSpawn :: EnemyId -> GoatSpawn
goatSpawn uuid =
  GoatSpawn
    $ baseAttrs uuid "01180"
    $ (healthDamage .~ 1)
    . (fight .~ 3)
    . (health .~ Static 3)
    . (evade .~ 2)

instance HasModifiersFor env GoatSpawn where
  getModifiersFor _ _ _ = pure []

instance HasModifiers env GoatSpawn where
  getModifiers _ (GoatSpawn Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env GoatSpawn where
  getActions i window (GoatSpawn attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GoatSpawn where
  runMessage msg (GoatSpawn attrs@Attrs {..}) = case msg of
    EnemyDefeated eid _ _ _ | eid == enemyId -> do
      investigatorIds <- asks $ setToList . getSet enemyLocation
      unshiftMessages
        [ InvestigatorAssignDamage iid (EnemySource eid) 0 1
        | iid <- investigatorIds
        ]
      GoatSpawn <$> runMessage msg attrs
    _ -> GoatSpawn <$> runMessage msg attrs
