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
  }

instance HasModifiersFor env investigator FleshEater where
  getModifiersFor _ _ = pure []

instance HasModifiers env FleshEater where
  getModifiers (FleshEater Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance (IsInvestigator investigator) => HasActions env investigator FleshEater where
  getActions i window (FleshEater attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env FleshEater where
  runMessage msg e@(FleshEater attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId ->
      e <$ spawnAt enemyId "01113"
    _ -> FleshEater <$> runMessage msg attrs
