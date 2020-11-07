{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.LitaChantler where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype LitaChantler = LitaChantler Attrs
  deriving newtype (Show, ToJSON, FromJSON)

litaChantler :: AssetId -> LitaChantler
litaChantler uuid = LitaChantler $ baseAttrs uuid "01117" $ do
  slots .= [AllySlot]
  health ?= 3
  sanity ?= 3

instance (HasId LocationId InvestigatorId env) => HasModifiersFor env LitaChantler where
  getModifiersFor _ (InvestigatorTarget iid) (LitaChantler Attrs {..}) = do
    locationId <- asks $ getId @LocationId iid
    case assetInvestigator of
      Nothing -> pure []
      Just ownerId -> do
        sameLocation <- asks $ (== locationId) . getId ownerId
        pure [ SkillModifier SkillCombat 1 | sameLocation ]
  getModifiersFor _ _ _ = pure []

instance HasActions env LitaChantler where
  getActions i window (LitaChantler x) = getActions i window x

instance (AssetRunner env) => RunMessage env LitaChantler where
  runMessage msg a@(LitaChantler attrs@Attrs {..}) = case msg of
    SuccessfulAttackEnemy iid eid -> case assetInvestigator of
      Just ownerId -> do
        locationId <- asks $ getId @LocationId ownerId
        locationInvestigatorIds <- asks $ setToList . getSet locationId
        traits <- asks $ setToList . getSet eid
        if iid `elem` locationInvestigatorIds && Monster `elem` traits
          then a <$ unshiftMessage
            (chooseOne
              iid
              [ Run
                [ UseCardAbility iid (AssetSource assetId) Nothing 1
                , AddModifiers
                  (EnemyTarget eid)
                  (AssetSource assetId)
                  [DamageTaken 1]
                ]
              , Continue "Do not use Lita Chantler's ability"
              ]
            )
          else pure a
      _ -> pure a
    AfterAttackEnemy _ eid -> a <$ unshiftMessage
      (RemoveAllModifiersOnTargetFrom (EnemyTarget eid) (AssetSource assetId))
    _ -> LitaChantler <$> runMessage msg attrs

