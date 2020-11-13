{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.IcyGhoul where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype IcyGhoul = IcyGhoul Attrs
  deriving newtype (Show, ToJSON, FromJSON)

icyGhoul :: EnemyId -> IcyGhoul
icyGhoul uuid =
  IcyGhoul
    $ baseAttrs uuid "01119"
    $ (healthDamage .~ 2)
    . (sanityDamage .~ 1)
    . (fight .~ 3)
    . (health .~ Static 4)
    . (evade .~ 4)

instance HasModifiersFor env IcyGhoul where
  getModifiersFor = noModifiersFor

instance HasModifiers env IcyGhoul where
  getModifiers _ (IcyGhoul Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env IcyGhoul where
  getActions i window (IcyGhoul attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env IcyGhoul where
  runMessage msg e@(IcyGhoul attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId ->
      e <$ spawnAt enemyId "Cellar"
    _ -> IcyGhoul <$> runMessage msg attrs
