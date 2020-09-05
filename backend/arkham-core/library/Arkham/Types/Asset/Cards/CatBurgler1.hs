{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.CatBurgler1 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Ability
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
import Arkham.Types.Window
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype CatBurgler1 = CatBurgler1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

catBurgler1 :: AssetId -> CatBurgler1
catBurgler1 uuid = CatBurgler1 $ (baseAttrs uuid "01055")
  { assetSlots = [AllySlot]
  , assetHealth = Just 2
  , assetSanity = Just 2
  }

instance IsInvestigator investigator => HasActions env investigator CatBurgler1 where
  getActions i NonFast (CatBurgler1 Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility (AssetSource assetId) 1 (ActionAbility 1 Nothing))
      | not assetExhausted
      ]
  getActions i window (CatBurgler1 x) = getActions i window x

instance (AssetRunner env) => RunMessage env CatBurgler1 where
  runMessage msg (CatBurgler1 attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage
        (AddModifiers
          (InvestigatorTarget iid)
          (AssetSource aid)
          [SkillModifier SkillAgility 1]
        )
      CatBurgler1 <$> runMessage msg attrs
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId -> do
      engagedEnemyIds <- HashSet.toList <$> asks (getSet iid)
      locationId <- asks (getId @LocationId iid)
      blockedLocationIds <- HashSet.map unBlockedLocationId <$> asks (getSet ())
      connectedLocationIds <- HashSet.map unConnectedLocationId
        <$> asks (getSet locationId)
      let
        unblockedConnectedLocationIds =
          HashSet.toList $ connectedLocationIds `difference` blockedLocationIds
      unshiftMessages
        $ [ DisengageEnemy iid eid | eid <- engagedEnemyIds ]
        <> [ Ask
               iid
               (ChooseOne
                 [ MoveAction iid lid False
                 | lid <- unblockedConnectedLocationIds
                 ]
               )
           | not (null unblockedConnectedLocationIds)
           ]
      pure $ CatBurgler1 $ attrs & exhausted .~ True
    _ -> CatBurgler1 <$> runMessage msg attrs
