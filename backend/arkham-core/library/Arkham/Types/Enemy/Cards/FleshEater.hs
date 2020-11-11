{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.FleshEater where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype FleshEater = FleshEater Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fleshEater :: EnemyId -> FleshEater
fleshEater uuid =
  FleshEater
    $ baseAttrs uuid "01118"
    $ (healthDamage .~ 1)
    . (sanityDamage .~ 2)
    . (fight .~ 4)
    . (health .~ Static 4)
    . (evade .~ 1)

instance HasModifiersFor env FleshEater where
  getModifiersFor = noModifiersFor

instance HasModifiers env FleshEater where
  getModifiers _ (FleshEater Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env FleshEater where
  getActions i window (FleshEater attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env FleshEater where
  runMessage msg e@(FleshEater attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId ->
      e <$ spawnAt enemyId "01113"
    _ -> FleshEater <$> runMessage msg attrs
