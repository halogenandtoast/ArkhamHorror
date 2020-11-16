{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.GhoulPriest where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype GhoulPriest = GhoulPriest Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ghoulPriest :: EnemyId -> GhoulPriest
ghoulPriest uuid =
  GhoulPriest
    $ baseAttrs uuid "01116"
    $ (healthDamage .~ 2)
    . (sanityDamage .~ 2)
    . (fight .~ 4)
    . (health .~ PerPlayer 5)
    . (evade .~ 4)
    . (prey .~ HighestSkill SkillCombat)

instance HasModifiersFor env GhoulPriest where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env GhoulPriest where
  getActions i window (GhoulPriest attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GhoulPriest where
  runMessage msg (GhoulPriest attrs) = GhoulPriest <$> runMessage msg attrs
