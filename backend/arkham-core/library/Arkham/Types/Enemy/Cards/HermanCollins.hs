{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Enemy.Cards.HermanCollins
  ( HermanCollins(..)
  , hermanCollins
  )
where

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
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 3)
    . (healthL .~ Static 4)
    . (evadeL .~ 4)
    . (uniqueL .~ True)

instance HasModifiersFor env HermanCollins where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env HermanCollins where
  getActions iid NonFast (HermanCollins attrs@Attrs {..}) =
    withBaseActions iid NonFast attrs $ do
      locationId <- getId @LocationId iid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (toSource attrs)
              1
              (ActionAbility
                (Just Parley)
                (Costs [ActionCost 1, DiscardCost 4 Nothing mempty])
              )
            )
        | locationId == enemyLocation
        ]
  getActions _ _ _ = pure []

instance EnemyRunner env => RunMessage env HermanCollins where
  runMessage msg e@(HermanCollins attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAt (Just iid) eid (LocationWithTitle "Graveyard")
    UseCardAbility _ source _ 1 | isSource attrs source ->
      e <$ unshiftMessage (AddToVictory $ toTarget attrs)
    _ -> HermanCollins <$> runMessage msg attrs
