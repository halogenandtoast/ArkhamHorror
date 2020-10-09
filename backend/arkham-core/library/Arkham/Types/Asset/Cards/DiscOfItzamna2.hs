{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.DiscOfItzamna2 where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window
import ClassyPrelude

newtype DiscOfItzamna2 = DiscOfItzamna2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

discOfItzamna2 :: AssetId -> DiscOfItzamna2
discOfItzamna2 uuid =
  DiscOfItzamna2 $ (baseAttrs uuid "01059") { assetSlots = [AccessorySlot] }

instance HasModifiersFor env investigator DiscOfItzamna2 where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env investigator => HasActions env investigator DiscOfItzamna2 where
  getActions i (WhenEnemySpawns YourLocation traits) (DiscOfItzamna2 Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ UseCardAbility
          (getId () i)
          (AssetSource assetId)
          Nothing
          1
      | Elite `notElem` traits
      ]
  getActions i window (DiscOfItzamna2 x) = getActions i window x

instance (AssetRunner env) => RunMessage env DiscOfItzamna2 where
  runMessage msg a@(DiscOfItzamna2 attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage
        (AddModifiers
          (InvestigatorTarget iid)
          (AssetSource aid)
          [SkillModifier SkillWillpower 1]
        )
      DiscOfItzamna2 <$> runMessage msg attrs
    UseCardAbility _ (AssetSource aid) _ 1 | aid == assetId -> do
      menemySpawnMessage <- withQueue $ \queue ->
        (queue, find ((== Just EnemySpawnMessage) . messageType) queue)
      a <$ case menemySpawnMessage of
        Just (EnemySpawn _ eid) ->
          unshiftMessages [Discard (AssetTarget aid), Discard (EnemyTarget eid)]
        _ -> pure ()
    _ -> DiscOfItzamna2 <$> runMessage msg attrs
