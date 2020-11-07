{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.BearTrap
  ( BearTrap(..)
  , bearTrap
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype BearTrap = BearTrap Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

bearTrap :: AssetId -> BearTrap
bearTrap uuid = BearTrap $ baseAttrs uuid "81020" $ pure ()

instance HasModifiersFor env BearTrap where
  getModifiersFor _ (EnemyTarget eid) (BearTrap Attrs {..})
    | Just eid == assetEnemy = pure [EnemyFight (-1), EnemyEvade (-1)]
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (FastAbility FastPlayerWindow)

instance HasActions env BearTrap where
  getActions iid FastPlayerWindow (BearTrap attrs) | ownedBy attrs iid =
    pure [ActivateCardAbilityAction iid (ability attrs)]
  getActions iid window (BearTrap x) = getActions iid window x

instance AssetRunner env => RunMessage env BearTrap where
  runMessage msg a@(BearTrap attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationId <- asks $ getId @LocationId iid
      a <$ unshiftMessage (AttachAsset assetId (LocationTarget locationId))
    EnemyMove eid _ lid | Just lid == assetLocation -> do
      isRougarou <- asks $ (== CardCode "81028") . getId eid
      a <$ when
        isRougarou
        (unshiftMessage (AttachAsset assetId (EnemyTarget eid)))
    _ -> BearTrap <$> runMessage msg attrs
