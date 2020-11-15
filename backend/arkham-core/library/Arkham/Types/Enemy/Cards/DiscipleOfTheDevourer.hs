{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.DiscipleOfTheDevourer where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype DiscipleOfTheDevourer = DiscipleOfTheDevourer Attrs
  deriving newtype (Show, ToJSON, FromJSON)

discipleOfTheDevourer :: EnemyId -> DiscipleOfTheDevourer
discipleOfTheDevourer uuid =
  DiscipleOfTheDevourer
    $ baseAttrs uuid "50041"
    $ (healthDamage .~ 1)
    . (fight .~ 3)
    . (health .~ Static 1)
    . (evade .~ 1)

instance HasModifiersFor env DiscipleOfTheDevourer where
  getModifiersFor = noModifiersFor

instance HasModifiers env DiscipleOfTheDevourer where
  getModifiers _ (DiscipleOfTheDevourer Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env DiscipleOfTheDevourer where
  getActions i window (DiscipleOfTheDevourer attrs) = getActions i window attrs

instance EnemyRunner env => RunMessage env DiscipleOfTheDevourer where
  runMessage msg e@(DiscipleOfTheDevourer attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId -> do
      farthestEmptyLocationIds <-
        asks $ map unFarthestLocationId . setToList . getSet
          (iid, EmptyLocation)
      e <$ spawnAtOneOf iid eid farthestEmptyLocationIds
    EnemySpawn (Just iid) _ eid | eid == enemyId -> do
      let
        messages = [PlaceDoomOnAgenda, InvestigatorPlaceCluesOnLocation iid 1]
      step <- asks $ unAgendaStep . getStep
      if step == 1
        then unshiftMessage (chooseOne iid messages)
        else unshiftMessages messages
      DiscipleOfTheDevourer <$> runMessage msg attrs
    _ -> DiscipleOfTheDevourer <$> runMessage msg attrs
