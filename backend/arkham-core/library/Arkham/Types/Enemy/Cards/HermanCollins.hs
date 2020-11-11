{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.HermanCollins where

import Arkham.Import

import Arkham.Types.Action hiding (Ability)
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner

newtype HermanCollins = HermanCollins Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hermanCollins :: EnemyId -> HermanCollins
hermanCollins uuid =
  HermanCollins
    $ baseAttrs uuid "01138"
    $ (healthDamage .~ 1)
    . (sanityDamage .~ 1)
    . (fight .~ 3)
    . (health .~ Static 4)
    . (evade .~ 4)

instance HasModifiersFor env HermanCollins where
  getModifiersFor = noModifiersFor

instance HasModifiers env HermanCollins where
  getModifiers _ (HermanCollins Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env HermanCollins where
  getActions iid NonFast (HermanCollins attrs@Attrs {..}) = do
    baseActions <- getActions iid NonFast attrs
    cardCount <- getCardCount iid
    locationId <- asks $ getId @LocationId iid
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             iid
             (mkAbility (EnemySource enemyId) 1 (ActionAbility 1 (Just Parley)))
         | cardCount >= 4 && locationId == enemyLocation
         ]
  getActions _ _ _ = pure []

instance EnemyRunner env => RunMessage env HermanCollins where
  runMessage msg e@(HermanCollins attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId -> e <$ spawnAt eid "01133"
    UseCardAbility iid (EnemySource eid) _ 1 | eid == enemyId ->
      e <$ unshiftMessages
        (replicate 4 (ChooseAndDiscardCard iid)
        <> [AddToVictory (EnemyTarget enemyId)]
        )
    _ -> HermanCollins <$> runMessage msg attrs
