{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.GhoulPriest where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Prey
import Arkham.Types.SkillType
import ClassyPrelude

newtype GhoulPriest = GhoulPriest Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ghoulPriest :: EnemyId -> GhoulPriest
ghoulPriest uuid = GhoulPriest $ (baseAttrs uuid "01116")
  { enemyHealthDamage = 2
  , enemySanityDamage = 2
  , enemyFight = 4
  , enemyHealth = PerPlayer 5
  , enemyEvade = 4
  , enemyVictory = Just 2
  , enemyPrey = HighestSkill SkillCombat
  }

instance (IsInvestigator investigator) => HasActions env investigator GhoulPriest where
  getActions i window (GhoulPriest attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GhoulPriest where
  runMessage msg (GhoulPriest attrs) = GhoulPriest <$> runMessage msg attrs
