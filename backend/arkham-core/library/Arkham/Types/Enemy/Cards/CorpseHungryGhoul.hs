{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.CorpseHungryGhoul where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype CorpseHungryGhoul = CorpseHungryGhoul Attrs
  deriving newtype (Show, ToJSON, FromJSON)

corpseHungryGhoul :: EnemyId -> CorpseHungryGhoul
corpseHungryGhoul uuid =
  CorpseHungryGhoul
    $ baseAttrs uuid "50022"
    $ (healthDamage .~ 2)
    . (sanityDamage .~ 2)
    . (fight .~ 4)
    . (health .~ Static 3)
    . (evade .~ 3)

instance HasModifiersFor env CorpseHungryGhoul where
  getModifiersFor = noModifiersFor

instance HasModifiers env CorpseHungryGhoul where
  getModifiers _ (CorpseHungryGhoul Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env CorpseHungryGhoul where
  getActions i window (CorpseHungryGhoul attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env CorpseHungryGhoul where
  runMessage msg e@(CorpseHungryGhoul attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId ->
      e <$ spawnAt enemyId "Bedroom"
    _ -> CorpseHungryGhoul <$> runMessage msg attrs
