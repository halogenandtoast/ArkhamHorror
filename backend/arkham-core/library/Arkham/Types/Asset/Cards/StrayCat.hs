{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.StrayCat where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype StrayCat = StrayCat Attrs
  deriving newtype (Show, ToJSON, FromJSON)

strayCat :: AssetId -> StrayCat
strayCat uuid = StrayCat
  $ (baseAttrs uuid "01076") { assetSlots = [AllySlot], assetHealth = Just 2 }

instance HasModifiersFor env investigator StrayCat where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator StrayCat where
  getActions i window (StrayCat a) | ownedBy a i = do
    baseActions <- getActions i window a
    let ability = mkAbility (toSource a) 1 (FastAbility window)
    pure $ baseActions <> [ActivateCardAbilityAction (getId () i) ability]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env StrayCat where
  runMessage msg a@(StrayCat attrs@Attrs {..}) = case msg of
    UseCardAbility iid (AssetSource aid) _ 1 | aid == assetId -> do
      locationId <- asks $ getId @LocationId (getInvestigator attrs)
      locationEnemyIds <- asks $ setToList . getSet locationId
      nonEliteEnemyIds <- filterM
        (asks . (notMember Elite .) . getSet)
        locationEnemyIds

      a <$ unshiftMessages
        [ Discard (AssetTarget aid)
        , chooseOne
          iid
          [ EnemyEvaded iid enemyId | enemyId <- nonEliteEnemyIds ]
        ]
    _ -> StrayCat <$> runMessage msg attrs
