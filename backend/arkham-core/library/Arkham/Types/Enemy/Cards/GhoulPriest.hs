{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.GhoulPriest where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Prey

newtype GhoulPriest = GhoulPriest Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ghoulPriest :: EnemyId -> GhoulPriest
ghoulPriest uuid = GhoulPriest $ (baseAttrs uuid "01116")
  { enemyHealthDamage = 2
  , enemySanityDamage = 2
  , enemyFight = 4
  , enemyHealth = PerPlayer 5
  , enemyEvade = 4
  , enemyPrey = HighestSkill SkillCombat
  }

instance HasModifiersFor env GhoulPriest where
  getModifiersFor _ _ _ = pure []

instance HasModifiers env GhoulPriest where
  getModifiers _ (GhoulPriest Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env GhoulPriest where
  getActions i window (GhoulPriest attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GhoulPriest where
  runMessage msg (GhoulPriest attrs) = GhoulPriest <$> runMessage msg attrs
