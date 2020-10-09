{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.StrayCat where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype StrayCat = StrayCat Attrs
  deriving newtype (Show, ToJSON, FromJSON)

strayCat :: AssetId -> StrayCat
strayCat uuid = StrayCat
  $ (baseAttrs uuid "01076") { assetSlots = [AllySlot], assetHealth = Just 2 }

instance HasModifiersFor env investigator StrayCat where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator StrayCat where
  getActions i window (StrayCat attrs@Attrs {..})
    | Just (getId () i) == assetInvestigator = do
      baseActions <- getActions i window attrs
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction
               (getId () i)
               (mkAbility (AssetSource assetId) 1 (FastAbility window))
           ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env StrayCat where
  runMessage msg a@(StrayCat attrs@Attrs {..}) = case msg of
    UseCardAbility iid (AssetSource aid) _ 1 | aid == assetId -> do
      locationId <- asks (getId @LocationId (getInvestigator attrs))
      locationEnemyIds <- HashSet.toList <$> asks (getSet locationId)
      nonEliteEnemyIds <- flip filterM locationEnemyIds $ \enemyId -> do
        traits <- asks (getSet enemyId)
        pure $ Elite `notMember` traits

      unshiftMessages
        [ Discard (AssetTarget aid)
        , Ask iid
          $ ChooseOne [ EnemyEvaded iid enemyId | enemyId <- nonEliteEnemyIds ]
        ]
      pure a
    _ -> StrayCat <$> runMessage msg attrs
