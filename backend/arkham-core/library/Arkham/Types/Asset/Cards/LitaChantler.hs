{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.LitaChantler where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype LitaChantler = LitaChantler Attrs
  deriving newtype (Show, ToJSON, FromJSON)

litaChantler :: AssetId -> LitaChantler
litaChantler uuid = LitaChantler $ (baseAttrs uuid "01117")
  { assetSlots = [AllySlot]
  , assetHealth = Just 3
  , assetSanity = Just 3
  }

instance (IsInvestigator investigator) => HasActions env investigator LitaChantler where
  getActions i window (LitaChantler x) = getActions i window x

instance (AssetRunner env) => RunMessage env LitaChantler where
  runMessage msg a@(LitaChantler attrs@Attrs {..}) = case msg of
    SuccessfulAttackEnemy iid eid -> case assetInvestigator of
      Just ownerId -> do
        locationId <- asks (getId @LocationId ownerId)
        locationInvestigatorIds <- HashSet.toList <$> asks (getSet locationId)
        if iid `elem` locationInvestigatorIds
          then a <$ unshiftMessage
            (Ask iid $ ChooseOne
              [ Run
                [ UseCardAbility
                  iid
                  (AssetSource assetId)
                  (AssetSource assetId)
                  1
                , AddModifier
                  (EnemyTarget eid)
                  (DamageTaken 1 (AssetSource assetId))
                ]
              , Continue "Do not use Lita Chantler's ability"
              ]
            )
          else pure a
      _ -> pure a
    AfterAttackEnemy _ eid -> a <$ unshiftMessage
      (RemoveAllModifiersOnTargetFrom (EnemyTarget eid) (AssetSource assetId))
    PostPlayerWindow -> do
      allInvestigatorIds <- HashSet.toList <$> asks (getSet ())
      case assetInvestigator of
        Just ownerId -> do
          locationId <- asks (getId @LocationId ownerId)
          locationInvestigatorIds <- HashSet.toList <$> asks (getSet locationId)
          unshiftMessages $ map
            (flip
                AddModifier
                (SkillModifier SkillCombat 1 (AssetSource assetId))
            . InvestigatorTarget
            )
            locationInvestigatorIds
        _ -> pure ()
      unshiftMessages $ map
        (\iid -> RemoveAllModifiersOnTargetFrom
          (InvestigatorTarget iid)
          (AssetSource assetId)
        )
        allInvestigatorIds
      pure a
    _ -> LitaChantler <$> runMessage msg attrs

