{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.RuthTurner where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Target
import ClassyPrelude

newtype RuthTurner = RuthTurner Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ruthTurner :: EnemyId -> RuthTurner
ruthTurner uuid = RuthTurner $ (baseAttrs uuid "01141")
  { enemyHealthDamage = 1
  , enemyFight = 2
  , enemyHealth = Static 4
  , enemyEvade = 5
  }

instance HasModifiersFor env RuthTurner where
  getModifiersFor _ _ _ = pure []

instance HasModifiers env RuthTurner where
  getModifiers _ (RuthTurner Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env RuthTurner where
  getActions i window (RuthTurner attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env RuthTurner where
  runMessage msg e@(RuthTurner attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId -> e <$ spawnAt eid "01128"
    EnemyEvaded _ eid | eid == enemyId ->
      e <$ unshiftMessage (AddToVictory (EnemyTarget enemyId))
    _ -> RuthTurner <$> runMessage msg attrs
