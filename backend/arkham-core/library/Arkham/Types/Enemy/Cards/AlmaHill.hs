{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.AlmaHill where

import Arkham.Import

import Arkham.Types.Action
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype AlmaHill = AlmaHill Attrs
  deriving newtype (Show, ToJSON, FromJSON)

almaHill :: EnemyId -> AlmaHill
almaHill uuid =
  AlmaHill
    $ baseAttrs uuid "50046"
    $ (sanityDamage .~ 2)
    . (fight .~ 3)
    . (health .~ Static 3)
    . (evade .~ 3)
    . (unique .~ True)

instance HasModifiersFor env AlmaHill where
  getModifiersFor = noModifiersFor

instance HasModifiers env AlmaHill where
  getModifiers _ (AlmaHill Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env AlmaHill where
  getActions iid NonFast (AlmaHill attrs@Attrs {..}) = do
    baseActions <- getActions iid NonFast attrs
    locationId <- asks $ getId @LocationId iid
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             iid
             (mkAbility (EnemySource enemyId) 1 (ActionAbility 1 (Just Parley)))
         | locationId == enemyLocation
         ]
  getActions _ _ _ = pure []

instance (EnemyRunner env) => RunMessage env AlmaHill where
  runMessage msg e@(AlmaHill attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAt (Just iid) eid "Southside"
    UseCardAbility iid (EnemySource eid) _ 1 | eid == enemyId ->
      e <$ unshiftMessages
        (replicate 3 (InvestigatorDrawEncounterCard iid)
        <> [AddToVictory (toTarget attrs)]
        )
    _ -> AlmaHill <$> runMessage msg attrs
