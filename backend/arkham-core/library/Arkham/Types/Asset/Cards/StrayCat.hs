{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.StrayCat where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype StrayCat = StrayCat Attrs
  deriving newtype (Show, ToJSON, FromJSON)

strayCat :: AssetId -> StrayCat
strayCat uuid = StrayCat $ baseAttrs uuid "01076" $ do
  slots .= [AllySlot]
  health ?= 2

instance HasModifiersFor env StrayCat where
  getModifiersFor _ _ _ = pure []

instance HasActions env StrayCat where
  getActions iid window (StrayCat a) | ownedBy a iid = do
    baseActions <- getActions iid window a
    let ability = mkAbility (toSource a) 1 (FastAbility window)
    pure $ baseActions <> [ActivateCardAbilityAction iid ability]
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
