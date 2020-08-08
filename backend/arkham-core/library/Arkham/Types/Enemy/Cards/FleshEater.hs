{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.FleshEater where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Message
import ClassyPrelude

newtype FleshEater = FleshEater Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fleshEater :: EnemyId -> FleshEater
fleshEater uuid = FleshEater $ (baseAttrs uuid "01118")
  { enemyHealthDamage = 1
  , enemySanityDamage = 2
  , enemyFight = 4
  , enemyHealth = Static 4
  , enemyEvade = 1
  , enemyVictory = Just 1
  }

instance (EnemyRunner env) => RunMessage env FleshEater where
  runMessage msg e@(FleshEater attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId ->
      e <$ spawnAt "01113" enemyId
    _ -> FleshEater <$> runMessage msg attrs
