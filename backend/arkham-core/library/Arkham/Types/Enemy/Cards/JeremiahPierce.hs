{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Enemy.Cards.JeremiahPierce
  ( JeremiahPierce(..)
  , jeremiahPierce
  )
where

import Arkham.Import

import Arkham.Types.Action hiding (Ability)
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers

newtype JeremiahPierce = JeremiahPierce Attrs
  deriving newtype (Show, ToJSON, FromJSON)

jeremiahPierce :: EnemyId -> JeremiahPierce
jeremiahPierce uuid =
  JeremiahPierce
    $ baseAttrs uuid "50044"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 4)
    . (healthL .~ Static 3)
    . (evadeL .~ 4)
    . (uniqueL .~ True)

instance HasModifiersFor env JeremiahPierce where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env JeremiahPierce where
  getActions iid NonFast (JeremiahPierce attrs@Attrs {..}) =
    withBaseActions iid NonFast attrs $ do
      locationId <- getId @LocationId iid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (EnemySource enemyId)
              1
              (ActionAbility (Just Parley) (ActionCost 1))
            )
        | locationId == enemyLocation
        ]
  getActions _ _ _ = pure []

instance (EnemyRunner env) => RunMessage env JeremiahPierce where
  runMessage msg e@(JeremiahPierce attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId -> do
      myourHouse <- getId @(Maybe LocationId) (LocationWithTitle "Your House")
      let
        spawnLocation =
          LocationWithTitle $ maybe "Rivertown" (const "Your House") myourHouse
      e <$ spawnAt (Just iid) eid spawnLocation
    UseCardAbility iid (EnemySource eid) _ 1 | eid == enemyId ->
      e <$ unshiftMessages
        [ AddToVictory (EnemyTarget enemyId)
        , CreateEffect
          enemyCardCode
          Nothing
          (toSource attrs)
          (InvestigatorTarget iid)
        ]
    _ -> JeremiahPierce <$> runMessage msg attrs
