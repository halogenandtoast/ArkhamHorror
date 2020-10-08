{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Aquinnah1 where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Window
import ClassyPrelude
import Lens.Micro

newtype Aquinnah1 = Aquinnah1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

aquinnah1 :: AssetId -> Aquinnah1
aquinnah1 uuid = Aquinnah1 $ (baseAttrs uuid "01082")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 4
  }

instance HasModifiersFor env investigator Aquinnah1 where
  getModifiersFor _ _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator Aquinnah1 where
  getActions i (WhenEnemyAttacks You) (Aquinnah1 Attrs {..})
    | Just (getId () i) == assetInvestigator = do
      enemyId <- withQueue $ \queue -> do
        let (_, rest) = break ((== Just AttackMessage) . messageType) queue
        case rest of
          (PerformEnemyAttack iid eid : _) | iid == getId () i -> (queue, eid)
          _ -> error "must be present"
      enemyIds <- filter (/= enemyId) . setToList <$> asks
        (getSet (locationOf i))
      pure
        [ ActivateCardAbilityAction
            (getId () i)
            (mkAbility
              (AssetSource assetId)
              1
              (FastAbility (WhenEnemyAttacks You))
            )
        | not assetExhausted && not (null enemyIds)
        ]
  getActions i window (Aquinnah1 x) = getActions i window x

instance (AssetRunner env) => RunMessage env Aquinnah1 where
  runMessage msg (Aquinnah1 attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId -> do
      enemyId <- withQueue $ \queue -> do
        let
          (_before, rest) = break ((== Just AttackMessage) . messageType) queue
        case rest of
          (PerformEnemyAttack _ eid : rest') -> (rest', eid)
          _ -> error "must be present"
      healthDamage' <- unHealthDamageCount <$> asks (getCount enemyId)
      sanityDamage' <- unSanityDamageCount <$> asks (getCount enemyId)
      locationId <- asks (getId @LocationId iid)
      enemyIds <- filter (/= enemyId) . setToList <$> asks (getSet locationId)

      unshiftMessage
        (Ask iid $ ChooseOne
          [ Run
              [ EnemyDamage eid iid (AssetSource assetId) healthDamage'
              , InvestigatorAssignDamage
                iid
                (EnemySource enemyId)
                0
                sanityDamage'
              ]
          | eid <- enemyIds
          ]
        )

      pure $ Aquinnah1 $ attrs & exhausted .~ True
    _ -> Aquinnah1 <$> runMessage msg attrs
