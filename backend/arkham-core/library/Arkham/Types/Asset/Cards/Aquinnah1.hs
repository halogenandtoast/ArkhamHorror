{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Aquinnah1
  ( Aquinnah1(..)
  , aquinnah1
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype Aquinnah1 = Aquinnah1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

aquinnah1 :: AssetId -> Aquinnah1
aquinnah1 uuid = Aquinnah1 $ baseAttrs uuid "01082" $ do
  slots .= [AllySlot]
  health ?= 1
  sanity ?= 4

reactionAbility :: Attrs -> Ability
reactionAbility attrs =
  mkAbility (toSource attrs) 1 (FastAbility (WhenEnemyAttacks You))

dropUntilAttack :: [Message] -> [Message]
dropUntilAttack = dropWhile (notElem AttackMessage . messageType)

instance HasModifiersFor env Aquinnah1 where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env Aquinnah1 where
  getActions iid (WhenEnemyAttacks You) (Aquinnah1 a) | ownedBy a iid = do
    locationId <- asks $ getId @LocationId iid
    enemyId <- fromQueue $ \queue ->
      let PerformEnemyAttack iid' eid : _ = dropUntilAttack queue
      in if iid' == iid then eid else error "mismatch"
    enemyIds <- asks $ filterSet (/= enemyId) . getSet locationId
    pure
      [ ActivateCardAbilityAction iid (reactionAbility a)
      | not (assetExhausted a) && not (null enemyIds)
      ]
  getActions i window (Aquinnah1 x) = getActions i window x

instance AssetRunner env => RunMessage env Aquinnah1 where
  runMessage msg (Aquinnah1 attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      enemyId <- withQueue $ \queue ->
        let PerformEnemyAttack _ eid : queue' = dropUntilAttack queue
        in (queue', eid)
      healthDamage' <- asks $ unHealthDamageCount . getCount enemyId
      sanityDamage' <- asks $ unSanityDamageCount . getCount enemyId
      locationId <- asks $ getId @LocationId iid
      enemyIds <- asks $ filter (/= enemyId) . setToList . getSet locationId

      when (null enemyIds) (error "other enemies had to be present")

      unshiftMessage $ chooseOne
        iid
        [ Run
            [ EnemyDamage eid iid source healthDamage'
            , InvestigatorAssignDamage iid (EnemySource enemyId) 0 sanityDamage'
            ]
        | eid <- enemyIds
        ]

      pure $ Aquinnah1 $ attrs & exhausted .~ True
    _ -> Aquinnah1 <$> runMessage msg attrs
