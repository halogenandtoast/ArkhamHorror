{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.PeterWarren where

import Arkham.Import

import Arkham.Types.Action hiding (Ability)
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner

newtype PeterWarren = PeterWarren Attrs
  deriving newtype (Show, ToJSON, FromJSON)

peterWarren :: EnemyId -> PeterWarren
peterWarren uuid = PeterWarren $ (baseAttrs uuid "01139")
  { enemyHealthDamage = 1
  , enemyFight = 2
  , enemyHealth = Static 3
  , enemyEvade = 3
  }

instance HasModifiersFor env PeterWarren where
  getModifiersFor _ _ _ = pure []

instance HasModifiers env PeterWarren where
  getModifiers _ (PeterWarren Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env PeterWarren where
  getActions iid NonFast (PeterWarren attrs@Attrs {..}) = do
    baseActions <- getActions iid NonFast attrs
    spendableClueCount <- getSpendableClueCount [iid]
    locationId <- asks $ getId @LocationId iid
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             iid
             (mkAbility (EnemySource enemyId) 1 (ActionAbility 1 (Just Parley)))
         | spendableClueCount >= 2 && locationId == enemyLocation
         ]
  getActions _ _ _ = pure []

instance (EnemyRunner env) => RunMessage env PeterWarren where
  runMessage msg e@(PeterWarren attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId -> e <$ spawnAt eid "01129"
    UseCardAbility iid (EnemySource eid) _ 1 | eid == enemyId ->
      e <$ unshiftMessages
        [SpendClues 2 [iid], AddToVictory (EnemyTarget enemyId)]
    _ -> PeterWarren <$> runMessage msg attrs
