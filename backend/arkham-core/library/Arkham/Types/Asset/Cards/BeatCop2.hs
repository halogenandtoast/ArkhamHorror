{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.BeatCop2 where

import ClassyPrelude

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
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype BeatCop2 = BeatCop2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

beatCop2 :: AssetId -> BeatCop2
beatCop2 uuid = BeatCop2 $ (baseAttrs uuid "01018")
  { assetSlots = [AllySlot]
  , assetHealth = Just 3
  , assetSanity = Just 2
  }

instance (IsInvestigator investigator) => HasActions env investigator BeatCop2 where
  getActions i _ (BeatCop2 Attrs {..}) | Just (getId () i) == assetInvestigator =
    pure
      [ UseCardAbility
          (getId () i)
          (AssetSource assetId)
          (AssetSource assetId)
          Nothing
          1
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env BeatCop2 where
  runMessage msg a@(BeatCop2 attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage
        (AddModifiers
          (InvestigatorTarget iid)
          (AssetSource aid)
          [SkillModifier SkillCombat 1]
        )
      pure a
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId -> do
      locationId <- asks (getId @LocationId (getInvestigator attrs))
      locationEnemyIds <- HashSet.toList <$> asks (getSet locationId)
      unshiftMessages
        [ AssetDamage assetId (AssetSource assetId) 1 0
        , Ask iid $ ChooseOne
          [ EnemyDamage eid iid (AssetSource assetId) 1
          | eid <- locationEnemyIds
          ]
        ]
      pure . BeatCop2 $ attrs & exhausted .~ True
    _ -> BeatCop2 <$> runMessage msg attrs
